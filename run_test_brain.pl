
% CLPFBA
% Copyright (C) 2017 Mauro DiNuzzo


:- use_module(library(clpr)).
:- use_module(clpfba_no_opt).


run_test_brain :-
  format('Brain Energy Metabolism (DiNuzzo et al, 2017).\n', []),
  (   project_files(Network, Constraints, N0),
      %N = 1000, % overriding <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
      N = N0,
      catch(clpfba('projects/brain', Network, Constraints, N), 
          Exception, 
          format('\n*** FAILED (no solutions) [Exception: ~w]\n\n', [Exception])
      ),
      fail
  ;   true
  ).
  
   

clpfba:labeling_hook(Model) :-
    format('Optimization "Neurotransmission (JNTn)" ... ', []),
    nth1(1, Model, _=JNTn),
    {F = JNTn},
    (   maximization_of(Model, F)
    -> format('succeeded.\n', [])
    ;   format('*** FAILED (no solutions).\n', [])
    ).
clpfba:labeling_hook(Model) :-
    format('Optimization "ATP Production (JPGKx+JPKx+JSCSx+JOPx)" ... ', []),
    nth1(29, Model, _=JPGKn),
    nth1(42, Model, _=JPGKa),    
    nth1(32, Model, _=JPKn),
    nth1(45, Model, _=JPKa),     
    nth1(58, Model, _=JSCSn),
    nth1(87, Model, _=JSCSa),     
    nth1(108, Model, _=JOPn),
    nth1(114, Model, _=JOPa),
    {F = JPGKn+JPGKa+JPKn+JPKa+JSCSn+JSCSa+JOPn+JOPa},
    (   maximization_of(Model, F)
    -> format('succeeded.\n', [])
    ;   format('*** FAILED (no solutions).\n', [])
    ).
clpfba:labeling_hook(Model) :-
    format('Optimization "Redox (GAPDHx+PDHx+IDH1x+AKGDHx+SDHx+mMDHx+GDHx)" ... ', []),
    nth1(28, Model, _=JGAPDHn),
    nth1(41, Model, _=JGAPDHa),    
    nth1(51, Model, _=JPDHn),
    nth1(80, Model, _=JPDHa),    
    nth1(54, Model, _=JIDH1n),
    nth1(83, Model, _=JIDH1a),    
    nth1(57, Model, _=JAKGDHn),
    nth1(86, Model, _=JAKGDHa),    
    nth1(58, Model, _=JSDHn),
    nth1(87, Model, _=JSDHa),    
    nth1(62, Model, _=JmMDHn),
    nth1(91, Model, _=JmMDHa),      
    nth1(73, Model, _=JGDHn),
    nth1(102, Model, _=JGDHa),                  
    {F = JGAPDHn+JPDHn+JIDH1n+JAKGDHn+JSDHn+JmMDHn+JGDHn+
           JGAPDHa+JPDHa+JIDH1a+JAKGDHa+JSDHa+JmMDHa+JGDHa},
    (   maximization_of(Model, F)
    -> format('succeeded.\n', [])
    ;   format('*** FAILED (no solutions).\n', [])
    ).    
    
maximization_of(Model, F) :-
    N is 100,
    sup(F, Sup),
    inf(F, Inf),
    Step is (Sup-Inf)/N,
    between(0, N, I),
    F0 is Sup-I*Step,
    {F >= F0}, 
    clpfba:labeling(Model), !.
    
   
   




  
project_files('network.pl', '1_core_model.pl', 10000).   % this is currently 63:58 (i.e. optimized to match experimental results)



project_files('network.pl', '0_awake.pl', 10000).



project_files('network_ions_20_20.pl', '1_core_model_ions_20_20.pl', 10000).
project_files('network_ions_40_40.pl', '1_core_model_ions_40_40.pl', 10000).
project_files('network_ions_60_60.pl', '1_core_model_ions_60_60.pl', 10000).
project_files('network_ions_80_80.pl', '1_core_model_ions_80_80.pl', 10000).
project_files('network_ions_100_100.pl', '1_core_model_ions_100_100.pl', 10000).

project_files('network_ions_60_44.pl', '1_core_model_ions_60_44.pl', 10000).
project_files('network_ions_60_52.pl', '1_core_model_ions_60_52.pl', 10000).
project_files('network_ions_60_68.pl', '1_core_model_ions_60_68.pl', 10000).
project_files('network_ions_60_76.pl', '1_core_model_ions_60_76.pl', 10000).

project_files('network_aros_025.pl', '1_core_model_aros_025.pl', 10000).
project_files('network_aros_050.pl', '1_core_model_aros_050.pl', 10000).
project_files('network_aros_100.pl', '1_core_model_aros_100.pl', 10000).  
project_files('network_aros_200.pl', '1_core_model_aros_200.pl', 10000). 



