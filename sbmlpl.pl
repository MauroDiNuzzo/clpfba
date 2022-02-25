
% Copyright (C) 2017 Mauro DiNuzzo

:- module(sbmlpl, [
        sbml_to_pl/2
        % TODO pl_to_sbml/2
    ]).


:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- use_module(clpfba).

sbml_to_pl(SbmlFile, PlFile) :-
    load_xml(SbmlFile, DOM, []),
    atomic_list_concat([PlFile, 'c'], CFile),
    setup_call_cleanup(
        (open(PlFile, write, Stream), open(CFile, write, CStream)),
        convert_sbml(Stream, CStream, DOM), 
        (close(Stream), close(CStream))
    ).
    
    
convert_sbml(Stream, CStream, DOM) :-
    xpath_chk(DOM, //model, Model),
    xpath_chk(Model, /self(@id), ModelId),
    xpath_chk(Model, /self(@name), ModelName),
    format(Stream, '; ~w\n; ~w\n\n', [ModelId, ModelName]),
    findall(Reaction, xpath(DOM, //reaction, Reaction), Reactions),
    length(Reactions, NumberOfReactions),
    nb_setval(error, none),
    (   between(1, NumberOfReactions, ReactionIndex),
        nb_getval(error, Error),
        (   Error = none
        -> true
        ;   throw(fatal_error(Error))
        ),
        nb_setval(error, nth1),
        nth1(ReactionIndex, Reactions, Reaction),
        nb_setval(error, xpath_chk_self_id),
        xpath_chk(Reaction, /self(@id), Id),
        format(Stream, 'J_~|~`0t~d~6+_~w :: ', [ReactionIndex, Id]),        
        nb_setval(error, xpath_chk_list_of_reactants),
        (   xpath_chk(Reaction, //listOfReactants, ListOfReactants)
        -> findall(Reference, xpath(ListOfReactants, //speciesReference, Reference), Reactants)
        ;   Reactants = []
        ),        
        nb_setval(error, convert_sbml_references(reactants)),
        convert_sbml_references(Stream, Reactants),
        nb_setval(error, xpath_chk_self_reversible),
        xpath_chk(Reaction, /self(@reversible), Reversible),
        nb_setval(error, convert_sbml_symbol),
        convert_sbml_symbol(Stream, Reversible),
        nb_setval(error, xpath_chk_list_of_products),
        (   xpath_chk(Reaction, //listOfProducts, ListOfProducts)
        -> findall(Reference, xpath(ListOfProducts, //speciesReference, Reference), Products)
        ;   Products = []
        ),
        nb_setval(error, convert_sbml_references(products)),
        convert_sbml_references(Stream, Products),       
        nb_setval(error, xpath_chk_self_name),
        xpath_chk(Reaction, /self(@name), Name),        
        format(Stream, ' ; ~w\n', [Name]),
        %
        nb_setval(error, xpath_chk_kinetic_law),
        xpath_chk(Reaction, //kineticLaw, KineticLaw),
        nb_setval(error, xpath_chk_list_of_parameters),
        xpath_chk(KineticLaw, //listOfParameters, ListOfParameters),     
        nb_setval(error, xpath_chk_id_lower_bound),
        (   xpath_chk(ListOfParameters, //parameter(@id='LOWER_BOUND'), LParameter),
            xpath_chk(LParameter, /self(@value), LowerBound)
        ->  format(CStream, 'J_~|~`0t~d~6+_~w >= ~w\n', [ReactionIndex, Id, LowerBound])     
        ;   true
        ),
        nb_setval(error, xpath_chk_id_upper_bound),
        (   xpath_chk(ListOfParameters, //parameter(@id='UPPER_BOUND'), UParameter),
            xpath_chk(UParameter, /self(@value), UpperBound)  
        ->    format(CStream, 'J_~|~`0t~d~6+_~w =< ~w\n', [ReactionIndex, Id, UpperBound])
        ;   true
        ),
        nb_setval(error, none),
        fail
    ;   true
    ).    
    
convert_sbml_symbol(Stream, true) :- !,
    format(Stream, ' = ', []).
convert_sbml_symbol(Stream, false) :- !,
    format(Stream, ' -> ', []).    
    
:- dynamic user:convert_sbml_species_hook/2.    
:- multifile user:convert_sbml_species_hook/2.
    
convert_sbml_references(Stream, []) :- !,
    format(Stream, '_', []).
convert_sbml_references(Stream, [Reference]) :- 
    xpath(Reference, /self(@species), Species0),
    downcase_atom(Species0, Species),
    user:convert_sbml_species_hook(Species, What), !,                           
    format(Stream, What, []).    
convert_sbml_references(Stream, References) :-
    length(References, NumberOfReferences),    
    (   between(1, NumberOfReferences, ReferenceIndex),
        nth1(ReferenceIndex, References, Reference),
        xpath(Reference, /self(@species), Species0),
        xpath(Reference, /self(@stoichiometry), Stoichiometry),
        (   ReferenceIndex = 1
        -> true
        ;   format(Stream, ' + ', [])
        ),
        downcase_atom(Species0, Species),
        atom_number(Stoichiometry, S),
        (  S =:= 1.0
        -> format(Stream, '~w', [Species])
        ;   format(Stream, '~w*~w', [Stoichiometry, Species])
        ),
        fail
    ;   true
    ).

    
    