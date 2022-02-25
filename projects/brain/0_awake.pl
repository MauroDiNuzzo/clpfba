		; 
			
    Vcyc = J_109_ae_SN
    Vcyc = 0.51   ; basal rate Oz et al (2004) awake rat brain <<<<<<<<<<<<<< NOTE "=" instead of "<"
    ; corresponding to PDHn = 1.16; PDHa = 0.30; PC = 0.18

		J_201_be_GLUT > 0
   
    ; 0.9*(0.10-0.20)*32 = 2.88-5.76 (approx 3-6)
		J_801_n_ATPase > 2
    J_801_n_ATPase < 4
		J_901_a_ATPase > 1
    J_901_a_ATPase < 2
    

        