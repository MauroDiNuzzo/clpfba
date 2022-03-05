
		
; Mass balance model of neuron-astrocyte metabolic interactions in the brain
; 
; (C) 2015 Mauro DiNuzzo. All Rights Reserved.



; NEUROTRANSMISSION (INCLUDING IONIC MOVEMENTS) AND GLU-GLN CYCLE

; Overall neuronal activity (neurotransmission + ionic fluxes)
J_101_n_NT :: glunv + 60*nae + 68*kn -> glue + 60*nan + 68*ke  	; from Attwell&Laughlin budget (affects vcyc vs jpdhn slope)

; Glutamate uptake by astrocytic EAATs 
J_102_ea_EAAT :: glue + 3*nae + ka -> gluac + 3*naa + ke 

; Vesicular glutamate transported (VGLUT) in neurons
J_103_n_VGLUT :: glunc + 1.5*atpn -> glunv + 1.5*adpn

; Na/K-ATPase (NKA)
J_104_n_NKA :: 3*nan + 2*ke + atpn -> 3*nae + 2*kn + adpn
J_105_a_NKA :: 3*naa + 2*ke + atpa -> 3*nae + 2*ka + adpa

; KIR channels (K buffering)
J_106_an_KIR :: ka -> kn

; Na channels (astrocyte) [indirectly incorporates NCX, NHE, NKCC, NBC]
J_107_a_Nax :: nae -> naa

; Glutamine synthetase (GS) in astrocytes
J_108_a_GS :: gluac + atpa + nh4a -> glna + adpa

; Glutamine release by astrocytes and uptake by neurons 
J_109_ae_SN :: glna -> glne           
J_110_en_SA :: glne + nae -> glnn + nan

; Phosphate-activated glutaminase (PAG) in neurons
J_111_n_PAG :: glnn -> glunc + nh4n  




; NUTRIENTS SUPPLY 

; Glucose (GLC) uptake from blood
J_201_be_GLUT :: _ -> glce
J_202_en_GLUT :: glce -> glcn
J_203_ea_GLUT :: glce -> glca
 
; Oxygen (O2) diffusion from blood
J_204_b_O2 :: _ -> o2b
J_205_bn_O2 :: o2b -> o2n
J_206_ba_O2 :: o2b -> o2a

; Carbon-dioxide diffusion to blood
J_207_nb_CO2 :: co2n -> co2b
J_208_ab_CO2 :: co2a -> co2b
J_209_b_CO2 :: co2b -> _

; Lactate (LAC) intercellular trafficking via MCTs (no influx/efflux from/to blood)	
J_210_ne_MCT :: lacn = lace
J_211_ae_MCT :: laca = lace




; GLYCOLYSIS 

; Hexokinase (HK)
J_401_n_HK :: glcn + atpn -> g6pn + adpn
J_501_a_HK :: glca + atpa -> g6pa + adpa

; Phosphoglucoisomerase (PGI)
J_402_n_PGI :: g6pn = f6pn
J_502_a_PGI :: g6pa = f6pa

; Phosphofructokinase (PFK)
J_403_n_PFK :: f6pn + atpn -> fbpn + adpn
J_503_a_PFK :: f6pa + atpa -> fbpa + adpa

; Alsolase (ALD)
J_404_n_ALD :: fbpn = gapn + dhapn
J_504_a_ALD :: fbpa = gapa + dhapa

; Triosephosphate isomerase (TPI)
J_405_n_TPI :: dhapn = gapn
J_505_a_TPI :: dhapa = gapa

; Glyceraldehyde-phosphate dehydrogenase (GAPDH)
J_406_n_GAPDH :: gapn + nadnc = bpgn + nadhnc		
J_506_a_GAPDH :: gapa + nadac = bpga + nadhac

; Phosphoglycerate kinase (PGK)
J_407_n_PGK :: bpgn + adpn = pg3n + atpn
J_507_a_PGK :: bpga + adpa = pg3a + atpa

; Phoshpglycerate mutase (PGM)
J_408_n_PGM :: pg3n = pg2n
J_508_a_PGM :: pg3a = pg2a

; Enolase (ENO)
J_409_n_ENO :: pg2n = pepn
J_509_a_ENO :: pg2a = pepa

; Pyruvate kinase (PK)
J_410_n_PK :: pepn + adpn -> pyrnc + atpn
J_510_a_PK :: pepa + adpa -> pyrac + atpa

; Lactate dehydrogenase (LDH)
J_411_nc_LDH :: pyrnc + nadhnc = lacn + nadnc		
J_511_ac_LDH :: pyrac + nadhac = laca + nadac		



; PYRUVATE RECYCLING AND ANAPLEROSIS

; Malic enzyme (ME)
J_412_nc_ME :: malnc + nadpn -> pyrnc + co2n + nadphn		
J_413_nm_ME :: malnm + nadpnm -> pyrnm + co2n + nadphnm	
J_512_ac_ME :: malac + nadpa -> pyrac + co2a + nadpha
J_513_am_ME :: malam + nadpam -> pyram + co2a + nadpham

; Pyruvate carboxylase (PC) in astrocytes [mitochondrial]
J_514_a_PC :: pyram + co2a + atpa -> oaaam + adpa




; TRICARBOXYLIC ACID CYCLE

; Pyruvate transport via MCTs
J_600_nm_MCT :: pyrnc = pyrnm
J_700_am_MCT :: pyrac = pyram

; Pyruvate dehydrogenase (PDH)
J_601_n_PDH :: pyrnm + coan + nadnm -> acoan + co2n + nadhnm
J_701_a_PDH :: pyram + coaa + nadam -> acoaa + co2a + nadham

; Citrate synthase (CS)
J_602_n_CS :: oaanm + acoan -> citnm + coan
J_702_a_CS :: oaaam + acoaa -> citam + coaa

; Aconitase (ACO)
J_603_n_ACO :: citnm = isonm
J_703_a_ACO :: citam = isoam
; in the brain is almost exclusively mitochondrial [Fariss2005]

; Isocitrate dehydrogenase (IDH)
J_604_n_IDH1 :: isonm + nadnm -> akgnm + co2n + nadhnm
J_605_n_IDH2 :: isonm + nadpnm -> akgnm + co2n + nadphnm
J_606_n_IDH3 :: isonc + nadpn -> akgnc + co2n + nadphn
J_704_a_IDH1 :: isoam + nadam -> akgam + co2a + nadham
J_705_a_IDH2 :: isoam + nadpam -> akgam + co2a + nadpham
J_706_a_IDH3 :: isoac + nadpa -> akgac + co2a + nadpha

; Alpha-ketoglutarate dehydrogenase (AKGDH)
J_607_n_AKGDH :: akgnm + coan + nadnm -> scoan + co2n + nadhnm
J_707_a_AKGDH :: akgam + coaa + nadam -> scoaa + co2a + nadham

; Succinyl-CoA synthetase (thiokinase)
J_608_n_SCS :: scoan + adpn = sucn + coan + atpn		
J_708_a_SCS :: scoaa + adpa = suca + coaa + atpa		

; Succinate dehydrogenase (SDH)
J_609_n_SDH :: sucn + (2/3)*nadnm = fumn + (2/3)*nadhnm	
J_709_a_SDH :: suca + (2/3)*nadam = fuma + (2/3)*nadham	

; Fumarase (FUM)
J_610_n_FUM :: fumn = malnm
J_710_a_FUM :: fuma = malam

; Malate dehydrogenase (mMDH) [also part of MAS, see below]
J_611_nc_MDH :: oaanc + nadhnc -> malnc + nadnc
J_612_nm_MDH :: malnm + nadnm -> oaanm + nadhnm
J_711_ac_MDH :: oaaac + nadhac -> malac + nadac	
J_712_am_MDH :: malam + nadam -> oaaam + nadham
	
	

	
; MITOCHONDRIAL CARRIERS	
	
; Glutamate-hydroxyl carrier (GC) 
J_613_n_GC :: glunm = glunc
J_713_a_GC :: gluam = gluac

; Dicarboxylate carrier (DCC)
J_614_n_DCC :: malnm = malnc
J_714_a_DCC :: malam = malac

; Citrate-Isocitrate carrier (CIC) [takes into account citrate transport via tricarboxylate carrier, TCC]
J_615_n_CIC1 :: isonm = isonc
J_715_a_CIC1 :: isoam = isoac
J_616_n_CIC2TCC :: citnm = citnc
J_716_a_CIC2TCC :: citam = citac


; FATTY ACID SYNTHESIS

; ATP-Citrate Lyase (ACL)
; acoa is cycled as it goes to fatty acid synthesis that are converted back to acoa (mitochondrial)
J_617_n_ACL :: citnc + atpn + coan -> oaanc + acoan + adpn      
J_717_a_ACL :: citac + atpa + coaa -> oaaac + acoaa + adpa    

; BEFORE release of citrate to blood  (*) [0.80*oaaac]
; (*) Sonnewald et al, 1991; Westergaard et al, J Neurochem 1994
; See also Fe3+ (ferric iron) [Moos et al, J Neurochem 2007] uptake and Al3+ (aluminium) [Nagasawa et al, 2005]clearance by the brain to bloodstream via citrate 
; citrate is a chelator also of Ca2+ and Mg2+



; MITOCHONDRIAL NADH SHUTTLES

; Glycerol-3-phosphate shuttle
J_618_n_G3PS :: nadhnc + (2/3)*nadnm -> nadnc + (2/3)*nadhnm	
J_718_a_G3PS :: nadhac + (2/3)*nadam -> nadac + (2/3)*nadham

; Malate-aspartate shuttle (MAS)

; Oxoglutarate carrier		
J_619_n_OGC :: malnc + akgnm -> malnm + akgnc
J_719_a_OGC :: malac + akgam -> malam + akgac

; Asparate-glutamate carrier (AGC1, alarar)
J_620_n_AGC :: glunc + aspnm -> glunm + aspnc
J_720_a_AGC :: gluac + aspam -> gluam + aspac

; Aspartate aminotransferase (AAT)
J_621_nc_AAT :: aspnc + akgnc = oaanc + glunc
J_622_nm_AAT :: aspnm + akgnm = oaanm + glunm
J_721_ac_AAT :: aspac + akgac = oaaac + gluac
J_722_am_AAT :: aspam + akgam = oaaam + gluam




; AMMONIA HOMEOSTASIS

; Glutamate dehydrogenase (GDH)
J_623_n_GDH :: glunm + nadnm = akgnm + nadhnm + nh4n
J_723_a_GDH :: gluam + nadam = akgam + nadham + nh4a

; Alanine aminotransferase (ALAT)

J_624_n_ALAT :: glunm + pyrnm = akgnm + alan
J_724_a_ALAT :: gluam + pyram = akgam + alaa
J_625_ne_ALA :: alan = alae
J_725_ae_ALA :: alaa = alae

; Branched-chain aminotransferase (BCAT)

J_626_n_BCAT :: glunm + bckan = akgnm + bcaan
J_726_a_BCAT :: gluam + bckaa = akgam + bcaaa
J_627_ne_BCKA :: bckan = bckae
J_628_ne_BCAA :: bcaan = bcaae
J_727_ae_BCKA :: bckaa = bckae
J_728_ae_BCAA :: bcaaa = bcaae




; RESPIRATION

; Oxidative phosphorylation
J_800_n_OP :: 2*nadhnm + 5*adpn + o2n -> 2*nadnm + 5*atpn + 0.025*rosn		; ROS production (increasing this decreases ogi)
J_900_a_OP :: 2*nadham + 5*adpa + o2a -> 2*nadam + 5*atpa + 0.250*rosa		; ROS production (increasing this decreases ogi)



; HOUSEKEEPING

; ATPase (housekeeping)
J_801_n_ATPase :: atpn -> adpn
J_901_a_ATPase :: atpa -> adpa




; ANTIOXIDANT SYSTEM

; Pentose phosphates pathway (PPP)
J_802_n_PPP :: 3*g6pn + 6*nadpn -> gapn + 2*f6pn + 6*nadphn + 3*co2n
J_902_a_PPP :: 3*g6pa + 6*nadpa -> gapa + 2*f6pa + 6*nadpha + 3*co2a 

; Glutathione-mediated ROS scavenging enzymes (glutathione reductase + peroxidase)
J_803_n_GR1 :: gssgn + nadphn -> 2*gshn + nadpn
J_804_n_GR2 :: gssgn + nadphnm -> 2*gshn + nadpnm
J_805_n_GPX :: 2*gshn + rosn -> gssgn

J_903_a_GR1 :: gssga + nadpha -> 2*gsha + nadpa
J_904_a_GR2 :: gssga + nadpham -> 2*gsha + nadpam
J_905_a_GPX :: 2*gsha + rosa -> gssga




