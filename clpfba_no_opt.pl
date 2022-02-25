
% CLPFBA
% Copyright (C) 2017 Mauro DiNuzzo

:- module(clpfba, [
    clpfba/4,
    op(1200, xfy, (::))
  ]).
  
:- set_prolog_flag(double_quotes, codes).     % migration from 6.x to 7.x
  
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(terms)).
:- use_module(library(clpr)).

:- use_module(library(filesex)).


:- prolog_load_context(directory, Directory),
    assert(user:file_search_path(clpfba, Directory)).
    
% flow_rate(Units, Limit, MinZero, MaxZero)    
% Define limiting flux and units
% Tells that a flux is considered zero between Min and Max
% Use 0 and 0 for classical FBA
flow_rate('a.u.', 1000, -0.0, 0.0).	% note: 	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic current_metabolite/2.

% current_flux(atom Name, list Direction)
% Direction is either [0, 1] for irreversible fluxes or [-1, 1] for reversible fluxes
:- dynamic current_flux/2.

build_model(InFile, AuxFile, OutFile) :- 
    open(InFile, read, Stream),
    (   repeat,
        read_line_to_codes(Stream, Codes),
        to_atom_formatted(Codes, Line),
        process_line(Line),
        Codes == end_of_file, !
    ),
    close(Stream),
    save_clp_model(AuxFile, OutFile).

include_constraints(File, OutStream) :-
    open(File, read, Stream),
    (   repeat,
        read_line_to_codes(Stream, Codes),
        to_atom_formatted(Codes, Line),
        (   Line \= ''
        -> format(OutStream, '~w,\n', [Line])
        ;   true
        ),
        Codes == end_of_file, !
    ),
    close(Stream).
        
to_atom_formatted(end_of_file, '') :- !.
to_atom_formatted(Codes, Line) :-
    (   once(append([Codes0, ";", _], Codes))
    -> true
    ;   Codes0 = Codes
    ),
    atom_codes(Atom, Codes0), 
    normalize_space(atom(Line), Atom).       
    
    
process_line('') :- !.    
process_line(Line) :- 
    catch(atom_to_term(Line, Term, Bindings), _, fail), !,
    maplist(call, Bindings), 
    (	set_flux(Term)
    ->  true
    ;   format('\nERROR on line (set_flux/1): "~w"\n', [Line]),
        throw(process_error)
    ).
process_line(Line) :-
    format('\nERROR on line (atom_to_term/3): "~w"\n', [Line]),
    throw(process_error).
    
    
save_clp_model(AuxFile, OutputFile) :- 
	findall(Flux, current_flux(Flux, _), Fs),
	list_to_ord_set(Fs, Fluxes),
	length(Fluxes, Length),	
	open(OutputFile, write, Stream),
	format(Stream, ':- use_module(library(clpr)).\n\n', []),
	format(Stream, 'model([', []),
	(	between(1, Length, This),
		nth1(This, Fluxes, Flux),
		format(Stream, '~q=~w', [Flux, Flux]),
		(This=Length -> true ; format(Stream, ', ', [])),
		fail
	;	true
	),
	format(Stream, ']) :-\n', []),	
	flow_rate(Units, Rate, Min, Max),	
	format(Stream, '\tRate is ~w,\t% units (~w)\n\t{\n', [Rate, Units]),
	% set limiting flux
	(	current_flux(Name, [Inf, Sup]),
		format(Stream, '\t\t~w -(~w*Rate) >= 0.0, ~w -(~w*Rate) =< 0.0,\n', [Name, Inf, Name, Sup]),
		fail
	;	true
	),
	% set mass balance equations
	(	current_metabolite(Metabolite, Balance),
		format(Stream, '\t\t~w -(~w) >= 0.0, ~w -(~w) =< 0.0,\t\t% ~w\n', [Balance, Min, Balance, Max, Metabolite]),
		fail
	;	true
	),
	% include constraint auxiliary file
  include_constraints(AuxFile, Stream),
	format(Stream, '\t\t0 = 0\n\t},\n', []),
	/*
	format(Stream, '\tcontracting([', []),
	(	between(1, Length, This),
		nth1(This, Fluxes, Flux),
		format(Stream, '~w', [Flux]),
		(This=Length -> true ; format(Stream, ', ', [])),
		fail
	;	true
	), 	
	format(Stream, ']).\n', []),	
	*/
	format(Stream, '\ttrue.\n', []),
	close(Stream).
    
    

% set_flux(+FluxName :: +Reactions)
% Possible reactions are:
%     _ -> Product (exactly one product; irreversible entry, source)
%     Substrate -> _ (exactly one substrate; irreversible exit, sink)
%     Substrates -> Products (arbitrary substrates, products and coefficients; irreversible)
%     _ = Product (exactly one product; reversible entry/exit, source/sink)
%     Subtrate = _ (exactly one substrate; reversible entry/exit, source/sink)
%     Substrates = Products (arbitrary substrates, products and coefficients; reversible)


%
set_flux(Flux :: Var -> Product) :- 
  var(Var), !,
  must_be(atom1, Flux),
  must_be(atom1, Product),
	assert(current_flux(Flux, [0, 1])),	
	flux(Flux, Product, +1).
set_flux(Flux :: Substrate -> Var) :-
  var(Var), !,
  must_be(atom1, Flux),
  must_be(atom1, Substrate),
	assert(current_flux(Flux, [0, 1])),	
	flux(Flux, Substrate, -1).	
set_flux(Flux :: Substrates -> Products) :- !,
  must_be(atom1, Flux),	
	assert(current_flux(Flux, [0, 1])),	
	flux(Flux, Substrates, -1),
	flux(Flux, Products, +1).
set_flux(Flux :: Var = Product) :-	
  var(Var), !,
  must_be(atom1, Flux),
  must_be(atom1, Product),
	assert(current_flux(Flux, [-1, 1])),	
	flux(Flux, Product, +1).
set_flux(Flux :: Substrate = Var) :-	
  var(Var), !,
  must_be(atom1, Flux),
  must_be(atom1, Substrate),
	assert(current_flux(Flux, [-1, 1])),	
	flux(Flux, Substrate, -1).	
set_flux(Flux :: Substrates = Products) :-	!,
  must_be(atom1, Flux),
	assert(current_flux(Flux, [-1, 1])),	
	flux(Flux, Substrates, -1),
	flux(Flux, Products, +1).

error:has_type(atom1, Atom) :-
  atom(Atom),
  Atom \= ''.
	
% flux/3
% flux(+Name, +Metabolites, +Direction)	
%
% Add the relevant fluxes to a given metabolites.
flux(Name, (Metabolites + N * Metabolite), Direction) :- !,
	Coefficient is N*Direction,
	metabolite_flux(Metabolite, Name, Coefficient),
	flux(Name, Metabolites, Direction).
flux(Name, (Metabolites + Metabolite), Direction) :- !,
	metabolite_flux(Metabolite, Name, Direction),
	flux(Name, Metabolites, Direction).	
flux(Name, (N * Metabolite), Direction) :- !,
	Coefficient is N*Direction,
	metabolite_flux(Metabolite, Name, Coefficient).	
flux(Name, (Metabolite), Direction) :-
	metabolite_flux(Metabolite, Name, Direction).

	
% metabolite_flux/3
% metabolite_flux(+Metabolite, +Name, +Coefficient)
%
% Add an individual flux to a metabolite. 
% Define the metabolite if it is not already present in the database.
metabolite_flux(Metabolite, Name, Coefficient) :-
  must_be(atom1, Metabolite),
	retract(current_metabolite(Metabolite, Balance)), !,
	atom_number(Atom, Coefficient),
	(	Coefficient > 0
	-> atom_concat(' +', Atom, OpCoeff)  % positive (add a '+' to the coefficient)
	;	atom_concat(' ', Atom, OpCoeff)  % negative (coefficient already contains a '-')
	),
	atomic_list_concat([Balance, OpCoeff, '*', Name], NewBalance),
	assert(current_metabolite(Metabolite, NewBalance)).
metabolite_flux(Metabolite, Name, Coefficient) :-
  must_be(atom1, Metabolite),
	atom_number(OpCoeff, Coefficient), % here no need to add a '+' (first argument)
	atomic_list_concat([OpCoeff, '*', Name], Balance),
	assert(current_metabolite(Metabolite, Balance)).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	
:- dynamic model/1.	

% clpfba(+Path, +FileName, +AuxFileName, +NumSolutions)
%
clpfba(Path, FileName, AuxFileName, N) :- 
  format('Running CLPFBA on "~w" [~w solutions]\n', [Path, N]),
  % init
  absolute_file_name(clpfba('./'), Directory, [file_type(directory)]),
  working_directory(_, Directory),
  retractall(solutions(_)),
  assert(solutions(0)),
  retractall(model(_)),
    retractall(current_metabolite(_, _)),
    retractall(current_flux(_, _)),  
  % validate/create output directory
  file_name_extension(Base, _, AuxFileName),
  directory_file_path(Path, Base, SavePath),
  catch(make_directory(SavePath), _, true),
  delete_directory_contents(SavePath),
  % build model
  format('Building model ... ', []),
	absolute_file_name(FileName, InputFile, [relative_to(Path)]),
  absolute_file_name(AuxFileName, AuxFile, [relative_to(Path)]),	
	absolute_file_name('output_model.pl', OutputFile, [relative_to(SavePath)]),
	build_model(InputFile, AuxFile, OutputFile),
	findall(FluxName, current_flux(FluxName, _), AllFluxes),
	findall(MetaboliteName, current_metabolite(MetaboliteName, _), AllMetabolites),
	length(AllFluxes, NAllFluxes),
	length(AllMetabolites, NAllMetabolites),
  format('done!\n', []),	
  format('Found ~w fluxes and ~w metabolites.\n', [NAllFluxes, NAllMetabolites]),
  absolute_file_name('output_fluxes.txt', OutputFluxesFile, [relative_to(SavePath)]),
  absolute_file_name('output_metabolites.txt', OutputMetabolitesFile, [relative_to(SavePath)]),
  save_info_file(OutputFluxesFile, AllFluxes),
  save_info_file(OutputMetabolitesFile, AllMetabolites),
  consult(OutputFile),  % here it may happen that there are syntax errors and file wont load
  solve(N, SavePath),
	% copy matlab utils
	%absolute_file_name(clpfba('matlab'), MatlabSource, [file_type(directory)]),
	%copy_directory(MatlabSource, SavePath),
	format('\nAll done!\nBye\n\n', []), !.
	

save_info_file(File, List) :-
  open(File, write, Stream),
  sort(List, Sorted),
  ( select(Item, Sorted, _),
    format(Stream, '~w~n', [Item]),
    fail
  ;  true
  ),
  close(Stream).

  
solve(N, SavePath) :-
	absolute_file_name('output_solutions.dat', DataFile, [relative_to(SavePath)]),
  absolute_file_name('output_domains.dat', DomainsFile, [relative_to(SavePath)]),  
  open(DataFile, write, DataStream),
  open(DomainsFile, write, DomainsStream), 
  format('Determining flux domains ... ', []),
  flush_output,
  (   model(Model)
  ->  true
  ;   throw(error(existence_error(model_solution, _), _))
  ),
  length(Model, Fluxes),
  format('done!\n', []),	  
  % save labels and domains
  (  between(1, Fluxes, Index),
      nth1(Index, Model, Name=_),
      format(DomainsStream, '~w\t', [Name]),
      fail
  ;   format(DomainsStream, '\n', [])
  ),
  (  between(1, Fluxes, Index),
      nth1(Index, Model, _=Flux),
      inf(Flux, Inf),      
      format(DomainsStream, '~w\t', [Inf]),
      fail
  ;   format(DomainsStream, '\n', [])
  ),
  (  between(1, Fluxes, Index),
      nth1(Index, Model, _=Flux),
      sup(Flux, Sup),      
      format(DomainsStream, '~w\t', [Sup]),
      fail
  ;   format(DomainsStream, '\n', [])
  ),  
  close(DomainsStream),
  format('done!\n', []),
  % find & save solutions
  format('Solving model (flux labeling). Enumerating solutions ...\n', []),
  flush_output,
  statistics(walltime, _),
	(	between(1, inf, Total),
        statistics(walltime, [_, StartTime]),
    		labeling(Model), 
    		statistics(walltime, [_, EndTime]),
    		Time is EndTime-StartTime,
		write_solution(DataStream, Model), 
		enough_solutions(N, Time)
	;	true
	),  
	Percent is integer(N/Total*100),
    format('Resolution success rate: ~w%.\n', [Percent]),
  close(DataStream).




labeling([]) :- !.
labeling(List) :-
  \+ term_variables(List, []), !,
  choose(List, _=Value, Rest),
  inf(Value, Inf),
  sup(Value, Sup),
  random(Inf, Sup, Value),
  labeling(Rest).
labeling(_).  
  
  
  
contracting(Vs) :- contracting(Vs, Vs).
 
contracting([], _).
 contracting([V|Vs], Vars) :-
 inf(V, Min),
 ( \+ \+ (V = Min) ->
 sup(V, Max),
 ( \+ \+ (V = Max) ->
 contracting(Vs, Vars)
 ; {V < Max},
 contracting(Vars, Vars)
 )
 ; {V > Min},
 contracting(Vars, Vars)
 ).
 
   
  
:- dynamic solutions/1.
  
enough_solutions(N, Time) :-
  retract(solutions(I)),
  I1 is I+1,
  assert(solutions(I1)),
  format('Found solution ~w/~w (~w msec)\n', [I1, N, Time]),
  flush_output,
  I1 >= N.
  
	
write_solution(Stream, []) :-
  nl(Stream), !.
write_solution(Stream, [_=Head|Tail]) :-
  format(Stream, '~w\t', [Head]),
  write_solution(Stream, Tail).

%% choose(List, Element, Rest) 
% Chooses a random element in List and unifies it with Element, while Rest is the remainder.
%
choose([], [], []) :- !.
choose(List, Element, Rest) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Element, Rest).
