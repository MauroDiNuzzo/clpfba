:- use_module(library(clpr)).

model(['J_101_n_NT'=J_101_n_NT, 'J_102_ea_EAAT'=J_102_ea_EAAT, 'J_103_n_VGLUT'=J_103_n_VGLUT, 'J_104_n_NKA'=J_104_n_NKA, 'J_105_a_NKA'=J_105_a_NKA, 'J_106_an_KIR'=J_106_an_KIR, 'J_107_a_Nax'=J_107_a_Nax, 'J_108_a_GS'=J_108_a_GS, 'J_109_ae_SN'=J_109_ae_SN, 'J_110_en_SA'=J_110_en_SA, 'J_111_n_PAG'=J_111_n_PAG, 'J_201_be_GLUT'=J_201_be_GLUT, 'J_202_en_GLUT'=J_202_en_GLUT, 'J_203_ea_GLUT'=J_203_ea_GLUT, 'J_204_b_O2'=J_204_b_O2, 'J_205_bn_O2'=J_205_bn_O2, 'J_206_ba_O2'=J_206_ba_O2, 'J_207_nb_CO2'=J_207_nb_CO2, 'J_208_ab_CO2'=J_208_ab_CO2, 'J_209_b_CO2'=J_209_b_CO2, 'J_210_ne_MCT'=J_210_ne_MCT, 'J_211_ae_MCT'=J_211_ae_MCT, 'J_401_n_HK'=J_401_n_HK, 'J_402_n_PGI'=J_402_n_PGI, 'J_403_n_PFK'=J_403_n_PFK, 'J_404_n_ALD'=J_404_n_ALD, 'J_405_n_TPI'=J_405_n_TPI, 'J_406_n_GAPDH'=J_406_n_GAPDH, 'J_407_n_PGK'=J_407_n_PGK, 'J_408_n_PGM'=J_408_n_PGM, 'J_409_n_ENO'=J_409_n_ENO, 'J_410_n_PK'=J_410_n_PK, 'J_411_nc_LDH'=J_411_nc_LDH, 'J_412_nc_ME'=J_412_nc_ME, 'J_413_nm_ME'=J_413_nm_ME, 'J_501_a_HK'=J_501_a_HK, 'J_502_a_PGI'=J_502_a_PGI, 'J_503_a_PFK'=J_503_a_PFK, 'J_504_a_ALD'=J_504_a_ALD, 'J_505_a_TPI'=J_505_a_TPI, 'J_506_a_GAPDH'=J_506_a_GAPDH, 'J_507_a_PGK'=J_507_a_PGK, 'J_508_a_PGM'=J_508_a_PGM, 'J_509_a_ENO'=J_509_a_ENO, 'J_510_a_PK'=J_510_a_PK, 'J_511_ac_LDH'=J_511_ac_LDH, 'J_512_ac_ME'=J_512_ac_ME, 'J_513_am_ME'=J_513_am_ME, 'J_514_a_PC'=J_514_a_PC, 'J_600_nm_MCT'=J_600_nm_MCT, 'J_601_n_PDH'=J_601_n_PDH, 'J_602_n_CS'=J_602_n_CS, 'J_603_n_ACO'=J_603_n_ACO, 'J_604_n_IDH1'=J_604_n_IDH1, 'J_605_n_IDH2'=J_605_n_IDH2, 'J_606_n_IDH3'=J_606_n_IDH3, 'J_607_n_AKGDH'=J_607_n_AKGDH, 'J_608_n_SCS'=J_608_n_SCS, 'J_609_n_SDH'=J_609_n_SDH, 'J_610_n_FUM'=J_610_n_FUM, 'J_611_nc_MDH'=J_611_nc_MDH, 'J_612_nm_MDH'=J_612_nm_MDH, 'J_613_n_GC'=J_613_n_GC, 'J_614_n_DCC'=J_614_n_DCC, 'J_615_n_CIC1'=J_615_n_CIC1, 'J_616_n_CIC2TCC'=J_616_n_CIC2TCC, 'J_617_n_ACL'=J_617_n_ACL, 'J_618_n_G3PS'=J_618_n_G3PS, 'J_619_n_OGC'=J_619_n_OGC, 'J_620_n_AGC'=J_620_n_AGC, 'J_621_nc_AAT'=J_621_nc_AAT, 'J_622_nm_AAT'=J_622_nm_AAT, 'J_623_n_GDH'=J_623_n_GDH, 'J_624_n_ALAT'=J_624_n_ALAT, 'J_625_ne_ALA'=J_625_ne_ALA, 'J_626_n_BCAT'=J_626_n_BCAT, 'J_627_ne_BCKA'=J_627_ne_BCKA, 'J_628_ne_BCAA'=J_628_ne_BCAA, 'J_700_am_MCT'=J_700_am_MCT, 'J_701_a_PDH'=J_701_a_PDH, 'J_702_a_CS'=J_702_a_CS, 'J_703_a_ACO'=J_703_a_ACO, 'J_704_a_IDH1'=J_704_a_IDH1, 'J_705_a_IDH2'=J_705_a_IDH2, 'J_706_a_IDH3'=J_706_a_IDH3, 'J_707_a_AKGDH'=J_707_a_AKGDH, 'J_708_a_SCS'=J_708_a_SCS, 'J_709_a_SDH'=J_709_a_SDH, 'J_710_a_FUM'=J_710_a_FUM, 'J_711_ac_MDH'=J_711_ac_MDH, 'J_712_am_MDH'=J_712_am_MDH, 'J_713_a_GC'=J_713_a_GC, 'J_714_a_DCC'=J_714_a_DCC, 'J_715_a_CIC1'=J_715_a_CIC1, 'J_716_a_CIC2TCC'=J_716_a_CIC2TCC, 'J_717_a_ACL'=J_717_a_ACL, 'J_718_a_G3PS'=J_718_a_G3PS, 'J_719_a_OGC'=J_719_a_OGC, 'J_720_a_AGC'=J_720_a_AGC, 'J_721_ac_AAT'=J_721_ac_AAT, 'J_722_am_AAT'=J_722_am_AAT, 'J_723_a_GDH'=J_723_a_GDH, 'J_724_a_ALAT'=J_724_a_ALAT, 'J_725_ae_ALA'=J_725_ae_ALA, 'J_726_a_BCAT'=J_726_a_BCAT, 'J_727_ae_BCKA'=J_727_ae_BCKA, 'J_728_ae_BCAA'=J_728_ae_BCAA, 'J_800_n_OP'=J_800_n_OP, 'J_801_n_ATPase'=J_801_n_ATPase, 'J_802_n_PPP'=J_802_n_PPP, 'J_803_n_GR1'=J_803_n_GR1, 'J_804_n_GR2'=J_804_n_GR2, 'J_805_n_GPX'=J_805_n_GPX, 'J_900_a_OP'=J_900_a_OP, 'J_901_a_ATPase'=J_901_a_ATPase, 'J_902_a_PPP'=J_902_a_PPP, 'J_903_a_GR1'=J_903_a_GR1, 'J_904_a_GR2'=J_904_a_GR2, 'J_905_a_GPX'=J_905_a_GPX]) :-
	Rate is 1000,	% units (a.u.)
	{
		J_101_n_NT -(0*Rate) >= 0.0, J_101_n_NT -(1*Rate) =< 0.0,
		J_102_ea_EAAT -(0*Rate) >= 0.0, J_102_ea_EAAT -(1*Rate) =< 0.0,
		J_103_n_VGLUT -(0*Rate) >= 0.0, J_103_n_VGLUT -(1*Rate) =< 0.0,
		J_104_n_NKA -(0*Rate) >= 0.0, J_104_n_NKA -(1*Rate) =< 0.0,
		J_105_a_NKA -(0*Rate) >= 0.0, J_105_a_NKA -(1*Rate) =< 0.0,
		J_106_an_KIR -(0*Rate) >= 0.0, J_106_an_KIR -(1*Rate) =< 0.0,
		J_107_a_Nax -(0*Rate) >= 0.0, J_107_a_Nax -(1*Rate) =< 0.0,
		J_108_a_GS -(0*Rate) >= 0.0, J_108_a_GS -(1*Rate) =< 0.0,
		J_109_ae_SN -(0*Rate) >= 0.0, J_109_ae_SN -(1*Rate) =< 0.0,
		J_110_en_SA -(0*Rate) >= 0.0, J_110_en_SA -(1*Rate) =< 0.0,
		J_111_n_PAG -(0*Rate) >= 0.0, J_111_n_PAG -(1*Rate) =< 0.0,
		J_201_be_GLUT -(0*Rate) >= 0.0, J_201_be_GLUT -(1*Rate) =< 0.0,
		J_202_en_GLUT -(0*Rate) >= 0.0, J_202_en_GLUT -(1*Rate) =< 0.0,
		J_203_ea_GLUT -(0*Rate) >= 0.0, J_203_ea_GLUT -(1*Rate) =< 0.0,
		J_204_b_O2 -(0*Rate) >= 0.0, J_204_b_O2 -(1*Rate) =< 0.0,
		J_205_bn_O2 -(0*Rate) >= 0.0, J_205_bn_O2 -(1*Rate) =< 0.0,
		J_206_ba_O2 -(0*Rate) >= 0.0, J_206_ba_O2 -(1*Rate) =< 0.0,
		J_207_nb_CO2 -(0*Rate) >= 0.0, J_207_nb_CO2 -(1*Rate) =< 0.0,
		J_208_ab_CO2 -(0*Rate) >= 0.0, J_208_ab_CO2 -(1*Rate) =< 0.0,
		J_209_b_CO2 -(0*Rate) >= 0.0, J_209_b_CO2 -(1*Rate) =< 0.0,
		J_210_ne_MCT -(-1*Rate) >= 0.0, J_210_ne_MCT -(1*Rate) =< 0.0,
		J_211_ae_MCT -(-1*Rate) >= 0.0, J_211_ae_MCT -(1*Rate) =< 0.0,
		J_401_n_HK -(0*Rate) >= 0.0, J_401_n_HK -(1*Rate) =< 0.0,
		J_501_a_HK -(0*Rate) >= 0.0, J_501_a_HK -(1*Rate) =< 0.0,
		J_402_n_PGI -(-1*Rate) >= 0.0, J_402_n_PGI -(1*Rate) =< 0.0,
		J_502_a_PGI -(-1*Rate) >= 0.0, J_502_a_PGI -(1*Rate) =< 0.0,
		J_403_n_PFK -(0*Rate) >= 0.0, J_403_n_PFK -(1*Rate) =< 0.0,
		J_503_a_PFK -(0*Rate) >= 0.0, J_503_a_PFK -(1*Rate) =< 0.0,
		J_404_n_ALD -(-1*Rate) >= 0.0, J_404_n_ALD -(1*Rate) =< 0.0,
		J_504_a_ALD -(-1*Rate) >= 0.0, J_504_a_ALD -(1*Rate) =< 0.0,
		J_405_n_TPI -(-1*Rate) >= 0.0, J_405_n_TPI -(1*Rate) =< 0.0,
		J_505_a_TPI -(-1*Rate) >= 0.0, J_505_a_TPI -(1*Rate) =< 0.0,
		J_406_n_GAPDH -(-1*Rate) >= 0.0, J_406_n_GAPDH -(1*Rate) =< 0.0,
		J_506_a_GAPDH -(-1*Rate) >= 0.0, J_506_a_GAPDH -(1*Rate) =< 0.0,
		J_407_n_PGK -(-1*Rate) >= 0.0, J_407_n_PGK -(1*Rate) =< 0.0,
		J_507_a_PGK -(-1*Rate) >= 0.0, J_507_a_PGK -(1*Rate) =< 0.0,
		J_408_n_PGM -(-1*Rate) >= 0.0, J_408_n_PGM -(1*Rate) =< 0.0,
		J_508_a_PGM -(-1*Rate) >= 0.0, J_508_a_PGM -(1*Rate) =< 0.0,
		J_409_n_ENO -(-1*Rate) >= 0.0, J_409_n_ENO -(1*Rate) =< 0.0,
		J_509_a_ENO -(-1*Rate) >= 0.0, J_509_a_ENO -(1*Rate) =< 0.0,
		J_410_n_PK -(0*Rate) >= 0.0, J_410_n_PK -(1*Rate) =< 0.0,
		J_510_a_PK -(0*Rate) >= 0.0, J_510_a_PK -(1*Rate) =< 0.0,
		J_411_nc_LDH -(-1*Rate) >= 0.0, J_411_nc_LDH -(1*Rate) =< 0.0,
		J_511_ac_LDH -(-1*Rate) >= 0.0, J_511_ac_LDH -(1*Rate) =< 0.0,
		J_412_nc_ME -(0*Rate) >= 0.0, J_412_nc_ME -(1*Rate) =< 0.0,
		J_413_nm_ME -(0*Rate) >= 0.0, J_413_nm_ME -(1*Rate) =< 0.0,
		J_512_ac_ME -(0*Rate) >= 0.0, J_512_ac_ME -(1*Rate) =< 0.0,
		J_513_am_ME -(0*Rate) >= 0.0, J_513_am_ME -(1*Rate) =< 0.0,
		J_514_a_PC -(0*Rate) >= 0.0, J_514_a_PC -(1*Rate) =< 0.0,
		J_600_nm_MCT -(-1*Rate) >= 0.0, J_600_nm_MCT -(1*Rate) =< 0.0,
		J_700_am_MCT -(-1*Rate) >= 0.0, J_700_am_MCT -(1*Rate) =< 0.0,
		J_601_n_PDH -(0*Rate) >= 0.0, J_601_n_PDH -(1*Rate) =< 0.0,
		J_701_a_PDH -(0*Rate) >= 0.0, J_701_a_PDH -(1*Rate) =< 0.0,
		J_602_n_CS -(0*Rate) >= 0.0, J_602_n_CS -(1*Rate) =< 0.0,
		J_702_a_CS -(0*Rate) >= 0.0, J_702_a_CS -(1*Rate) =< 0.0,
		J_603_n_ACO -(-1*Rate) >= 0.0, J_603_n_ACO -(1*Rate) =< 0.0,
		J_703_a_ACO -(-1*Rate) >= 0.0, J_703_a_ACO -(1*Rate) =< 0.0,
		J_604_n_IDH1 -(0*Rate) >= 0.0, J_604_n_IDH1 -(1*Rate) =< 0.0,
		J_605_n_IDH2 -(0*Rate) >= 0.0, J_605_n_IDH2 -(1*Rate) =< 0.0,
		J_606_n_IDH3 -(0*Rate) >= 0.0, J_606_n_IDH3 -(1*Rate) =< 0.0,
		J_704_a_IDH1 -(0*Rate) >= 0.0, J_704_a_IDH1 -(1*Rate) =< 0.0,
		J_705_a_IDH2 -(0*Rate) >= 0.0, J_705_a_IDH2 -(1*Rate) =< 0.0,
		J_706_a_IDH3 -(0*Rate) >= 0.0, J_706_a_IDH3 -(1*Rate) =< 0.0,
		J_607_n_AKGDH -(0*Rate) >= 0.0, J_607_n_AKGDH -(1*Rate) =< 0.0,
		J_707_a_AKGDH -(0*Rate) >= 0.0, J_707_a_AKGDH -(1*Rate) =< 0.0,
		J_608_n_SCS -(-1*Rate) >= 0.0, J_608_n_SCS -(1*Rate) =< 0.0,
		J_708_a_SCS -(-1*Rate) >= 0.0, J_708_a_SCS -(1*Rate) =< 0.0,
		J_609_n_SDH -(-1*Rate) >= 0.0, J_609_n_SDH -(1*Rate) =< 0.0,
		J_709_a_SDH -(-1*Rate) >= 0.0, J_709_a_SDH -(1*Rate) =< 0.0,
		J_610_n_FUM -(-1*Rate) >= 0.0, J_610_n_FUM -(1*Rate) =< 0.0,
		J_710_a_FUM -(-1*Rate) >= 0.0, J_710_a_FUM -(1*Rate) =< 0.0,
		J_611_nc_MDH -(0*Rate) >= 0.0, J_611_nc_MDH -(1*Rate) =< 0.0,
		J_612_nm_MDH -(0*Rate) >= 0.0, J_612_nm_MDH -(1*Rate) =< 0.0,
		J_711_ac_MDH -(0*Rate) >= 0.0, J_711_ac_MDH -(1*Rate) =< 0.0,
		J_712_am_MDH -(0*Rate) >= 0.0, J_712_am_MDH -(1*Rate) =< 0.0,
		J_613_n_GC -(-1*Rate) >= 0.0, J_613_n_GC -(1*Rate) =< 0.0,
		J_713_a_GC -(-1*Rate) >= 0.0, J_713_a_GC -(1*Rate) =< 0.0,
		J_614_n_DCC -(-1*Rate) >= 0.0, J_614_n_DCC -(1*Rate) =< 0.0,
		J_714_a_DCC -(-1*Rate) >= 0.0, J_714_a_DCC -(1*Rate) =< 0.0,
		J_615_n_CIC1 -(-1*Rate) >= 0.0, J_615_n_CIC1 -(1*Rate) =< 0.0,
		J_715_a_CIC1 -(-1*Rate) >= 0.0, J_715_a_CIC1 -(1*Rate) =< 0.0,
		J_616_n_CIC2TCC -(-1*Rate) >= 0.0, J_616_n_CIC2TCC -(1*Rate) =< 0.0,
		J_716_a_CIC2TCC -(-1*Rate) >= 0.0, J_716_a_CIC2TCC -(1*Rate) =< 0.0,
		J_617_n_ACL -(0*Rate) >= 0.0, J_617_n_ACL -(1*Rate) =< 0.0,
		J_717_a_ACL -(0*Rate) >= 0.0, J_717_a_ACL -(1*Rate) =< 0.0,
		J_618_n_G3PS -(0*Rate) >= 0.0, J_618_n_G3PS -(1*Rate) =< 0.0,
		J_718_a_G3PS -(0*Rate) >= 0.0, J_718_a_G3PS -(1*Rate) =< 0.0,
		J_619_n_OGC -(0*Rate) >= 0.0, J_619_n_OGC -(1*Rate) =< 0.0,
		J_719_a_OGC -(0*Rate) >= 0.0, J_719_a_OGC -(1*Rate) =< 0.0,
		J_620_n_AGC -(0*Rate) >= 0.0, J_620_n_AGC -(1*Rate) =< 0.0,
		J_720_a_AGC -(0*Rate) >= 0.0, J_720_a_AGC -(1*Rate) =< 0.0,
		J_621_nc_AAT -(-1*Rate) >= 0.0, J_621_nc_AAT -(1*Rate) =< 0.0,
		J_622_nm_AAT -(-1*Rate) >= 0.0, J_622_nm_AAT -(1*Rate) =< 0.0,
		J_721_ac_AAT -(-1*Rate) >= 0.0, J_721_ac_AAT -(1*Rate) =< 0.0,
		J_722_am_AAT -(-1*Rate) >= 0.0, J_722_am_AAT -(1*Rate) =< 0.0,
		J_623_n_GDH -(-1*Rate) >= 0.0, J_623_n_GDH -(1*Rate) =< 0.0,
		J_723_a_GDH -(-1*Rate) >= 0.0, J_723_a_GDH -(1*Rate) =< 0.0,
		J_624_n_ALAT -(-1*Rate) >= 0.0, J_624_n_ALAT -(1*Rate) =< 0.0,
		J_724_a_ALAT -(-1*Rate) >= 0.0, J_724_a_ALAT -(1*Rate) =< 0.0,
		J_625_ne_ALA -(-1*Rate) >= 0.0, J_625_ne_ALA -(1*Rate) =< 0.0,
		J_725_ae_ALA -(-1*Rate) >= 0.0, J_725_ae_ALA -(1*Rate) =< 0.0,
		J_626_n_BCAT -(-1*Rate) >= 0.0, J_626_n_BCAT -(1*Rate) =< 0.0,
		J_726_a_BCAT -(-1*Rate) >= 0.0, J_726_a_BCAT -(1*Rate) =< 0.0,
		J_627_ne_BCKA -(-1*Rate) >= 0.0, J_627_ne_BCKA -(1*Rate) =< 0.0,
		J_628_ne_BCAA -(-1*Rate) >= 0.0, J_628_ne_BCAA -(1*Rate) =< 0.0,
		J_727_ae_BCKA -(-1*Rate) >= 0.0, J_727_ae_BCKA -(1*Rate) =< 0.0,
		J_728_ae_BCAA -(-1*Rate) >= 0.0, J_728_ae_BCAA -(1*Rate) =< 0.0,
		J_800_n_OP -(0*Rate) >= 0.0, J_800_n_OP -(1*Rate) =< 0.0,
		J_900_a_OP -(0*Rate) >= 0.0, J_900_a_OP -(1*Rate) =< 0.0,
		J_801_n_ATPase -(0*Rate) >= 0.0, J_801_n_ATPase -(1*Rate) =< 0.0,
		J_901_a_ATPase -(0*Rate) >= 0.0, J_901_a_ATPase -(1*Rate) =< 0.0,
		J_802_n_PPP -(0*Rate) >= 0.0, J_802_n_PPP -(1*Rate) =< 0.0,
		J_902_a_PPP -(0*Rate) >= 0.0, J_902_a_PPP -(1*Rate) =< 0.0,
		J_803_n_GR1 -(0*Rate) >= 0.0, J_803_n_GR1 -(1*Rate) =< 0.0,
		J_804_n_GR2 -(0*Rate) >= 0.0, J_804_n_GR2 -(1*Rate) =< 0.0,
		J_805_n_GPX -(0*Rate) >= 0.0, J_805_n_GPX -(1*Rate) =< 0.0,
		J_903_a_GR1 -(0*Rate) >= 0.0, J_903_a_GR1 -(1*Rate) =< 0.0,
		J_904_a_GR2 -(0*Rate) >= 0.0, J_904_a_GR2 -(1*Rate) =< 0.0,
		J_905_a_GPX -(0*Rate) >= 0.0, J_905_a_GPX -(1*Rate) =< 0.0,
		1*J_101_n_NT -1*J_102_ea_EAAT -(-0.0) >= 0.0, 1*J_101_n_NT -1*J_102_ea_EAAT -(0.0) =< 0.0,		% glue
		-1*J_101_n_NT +1*J_103_n_VGLUT -(-0.0) >= 0.0, -1*J_101_n_NT +1*J_103_n_VGLUT -(0.0) =< 0.0,		% glunv
		76*J_101_n_NT +1*J_102_ea_EAAT -2*J_104_n_NKA -2*J_105_a_NKA -(-0.0) >= 0.0, 76*J_101_n_NT +1*J_102_ea_EAAT -2*J_104_n_NKA -2*J_105_a_NKA -(0.0) =< 0.0,		% ke
		-1*J_102_ea_EAAT +2*J_105_a_NKA -1*J_106_an_KIR -(-0.0) >= 0.0, -1*J_102_ea_EAAT +2*J_105_a_NKA -1*J_106_an_KIR -(0.0) =< 0.0,		% ka
		-76*J_101_n_NT +2*J_104_n_NKA +1*J_106_an_KIR -(-0.0) >= 0.0, -76*J_101_n_NT +2*J_104_n_NKA +1*J_106_an_KIR -(0.0) =< 0.0,		% kn
		3*J_102_ea_EAAT -3*J_105_a_NKA +1*J_107_a_Nax -(-0.0) >= 0.0, 3*J_102_ea_EAAT -3*J_105_a_NKA +1*J_107_a_Nax -(0.0) =< 0.0,		% naa
		1*J_108_a_GS -1*J_109_ae_SN -(-0.0) >= 0.0, 1*J_108_a_GS -1*J_109_ae_SN -(0.0) =< 0.0,		% glna
		-60*J_101_n_NT -3*J_102_ea_EAAT +3*J_104_n_NKA +3*J_105_a_NKA -1*J_107_a_Nax -1*J_110_en_SA -(-0.0) >= 0.0, -60*J_101_n_NT -3*J_102_ea_EAAT +3*J_104_n_NKA +3*J_105_a_NKA -1*J_107_a_Nax -1*J_110_en_SA -(0.0) =< 0.0,		% nae
		1*J_109_ae_SN -1*J_110_en_SA -(-0.0) >= 0.0, 1*J_109_ae_SN -1*J_110_en_SA -(0.0) =< 0.0,		% glne
		60*J_101_n_NT -3*J_104_n_NKA +1*J_110_en_SA -(-0.0) >= 0.0, 60*J_101_n_NT -3*J_104_n_NKA +1*J_110_en_SA -(0.0) =< 0.0,		% nan
		1*J_110_en_SA -1*J_111_n_PAG -(-0.0) >= 0.0, 1*J_110_en_SA -1*J_111_n_PAG -(0.0) =< 0.0,		% glnn
		1*J_201_be_GLUT -1*J_202_en_GLUT -1*J_203_ea_GLUT -(-0.0) >= 0.0, 1*J_201_be_GLUT -1*J_202_en_GLUT -1*J_203_ea_GLUT -(0.0) =< 0.0,		% glce
		1*J_204_b_O2 -1*J_205_bn_O2 -1*J_206_ba_O2 -(-0.0) >= 0.0, 1*J_204_b_O2 -1*J_205_bn_O2 -1*J_206_ba_O2 -(0.0) =< 0.0,		% o2b
		1*J_207_nb_CO2 +1*J_208_ab_CO2 -1*J_209_b_CO2 -(-0.0) >= 0.0, 1*J_207_nb_CO2 +1*J_208_ab_CO2 -1*J_209_b_CO2 -(0.0) =< 0.0,		% co2b
		1*J_210_ne_MCT +1*J_211_ae_MCT -(-0.0) >= 0.0, 1*J_210_ne_MCT +1*J_211_ae_MCT -(0.0) =< 0.0,		% lace
		1*J_202_en_GLUT -1*J_401_n_HK -(-0.0) >= 0.0, 1*J_202_en_GLUT -1*J_401_n_HK -(0.0) =< 0.0,		% glcn
		1*J_203_ea_GLUT -1*J_501_a_HK -(-0.0) >= 0.0, 1*J_203_ea_GLUT -1*J_501_a_HK -(0.0) =< 0.0,		% glca
		1*J_403_n_PFK -1*J_404_n_ALD -(-0.0) >= 0.0, 1*J_403_n_PFK -1*J_404_n_ALD -(0.0) =< 0.0,		% fbpn
		1*J_503_a_PFK -1*J_504_a_ALD -(-0.0) >= 0.0, 1*J_503_a_PFK -1*J_504_a_ALD -(0.0) =< 0.0,		% fbpa
		1*J_404_n_ALD -1*J_405_n_TPI -(-0.0) >= 0.0, 1*J_404_n_ALD -1*J_405_n_TPI -(0.0) =< 0.0,		% dhapn
		1*J_504_a_ALD -1*J_505_a_TPI -(-0.0) >= 0.0, 1*J_504_a_ALD -1*J_505_a_TPI -(0.0) =< 0.0,		% dhapa
		1*J_406_n_GAPDH -1*J_407_n_PGK -(-0.0) >= 0.0, 1*J_406_n_GAPDH -1*J_407_n_PGK -(0.0) =< 0.0,		% bpgn
		1*J_506_a_GAPDH -1*J_507_a_PGK -(-0.0) >= 0.0, 1*J_506_a_GAPDH -1*J_507_a_PGK -(0.0) =< 0.0,		% bpga
		1*J_407_n_PGK -1*J_408_n_PGM -(-0.0) >= 0.0, 1*J_407_n_PGK -1*J_408_n_PGM -(0.0) =< 0.0,		% pg3n
		1*J_507_a_PGK -1*J_508_a_PGM -(-0.0) >= 0.0, 1*J_507_a_PGK -1*J_508_a_PGM -(0.0) =< 0.0,		% pg3a
		1*J_408_n_PGM -1*J_409_n_ENO -(-0.0) >= 0.0, 1*J_408_n_PGM -1*J_409_n_ENO -(0.0) =< 0.0,		% pg2n
		1*J_508_a_PGM -1*J_509_a_ENO -(-0.0) >= 0.0, 1*J_508_a_PGM -1*J_509_a_ENO -(0.0) =< 0.0,		% pg2a
		1*J_409_n_ENO -1*J_410_n_PK -(-0.0) >= 0.0, 1*J_409_n_ENO -1*J_410_n_PK -(0.0) =< 0.0,		% pepn
		1*J_509_a_ENO -1*J_510_a_PK -(-0.0) >= 0.0, 1*J_509_a_ENO -1*J_510_a_PK -(0.0) =< 0.0,		% pepa
		-1*J_210_ne_MCT +1*J_411_nc_LDH -(-0.0) >= 0.0, -1*J_210_ne_MCT +1*J_411_nc_LDH -(0.0) =< 0.0,		% lacn
		-1*J_211_ae_MCT +1*J_511_ac_LDH -(-0.0) >= 0.0, -1*J_211_ae_MCT +1*J_511_ac_LDH -(0.0) =< 0.0,		% laca
		1*J_410_n_PK -1*J_411_nc_LDH +1*J_412_nc_ME -1*J_600_nm_MCT -(-0.0) >= 0.0, 1*J_410_n_PK -1*J_411_nc_LDH +1*J_412_nc_ME -1*J_600_nm_MCT -(0.0) =< 0.0,		% pyrnc
		1*J_510_a_PK -1*J_511_ac_LDH +1*J_512_ac_ME -1*J_700_am_MCT -(-0.0) >= 0.0, 1*J_510_a_PK -1*J_511_ac_LDH +1*J_512_ac_ME -1*J_700_am_MCT -(0.0) =< 0.0,		% pyrac
		1*J_607_n_AKGDH -1*J_608_n_SCS -(-0.0) >= 0.0, 1*J_607_n_AKGDH -1*J_608_n_SCS -(0.0) =< 0.0,		% scoan
		1*J_707_a_AKGDH -1*J_708_a_SCS -(-0.0) >= 0.0, 1*J_707_a_AKGDH -1*J_708_a_SCS -(0.0) =< 0.0,		% scoaa
		1*J_608_n_SCS -1*J_609_n_SDH -(-0.0) >= 0.0, 1*J_608_n_SCS -1*J_609_n_SDH -(0.0) =< 0.0,		% sucn
		1*J_708_a_SCS -1*J_709_a_SDH -(-0.0) >= 0.0, 1*J_708_a_SCS -1*J_709_a_SDH -(0.0) =< 0.0,		% suca
		1*J_609_n_SDH -1*J_610_n_FUM -(-0.0) >= 0.0, 1*J_609_n_SDH -1*J_610_n_FUM -(0.0) =< 0.0,		% fumn
		1*J_709_a_SDH -1*J_710_a_FUM -(-0.0) >= 0.0, 1*J_709_a_SDH -1*J_710_a_FUM -(0.0) =< 0.0,		% fuma
		1*J_603_n_ACO -1*J_604_n_IDH1 -1*J_605_n_IDH2 -1*J_615_n_CIC1 -(-0.0) >= 0.0, 1*J_603_n_ACO -1*J_604_n_IDH1 -1*J_605_n_IDH2 -1*J_615_n_CIC1 -(0.0) =< 0.0,		% isonm
		-1*J_606_n_IDH3 +1*J_615_n_CIC1 -(-0.0) >= 0.0, -1*J_606_n_IDH3 +1*J_615_n_CIC1 -(0.0) =< 0.0,		% isonc
		1*J_703_a_ACO -1*J_704_a_IDH1 -1*J_705_a_IDH2 -1*J_715_a_CIC1 -(-0.0) >= 0.0, 1*J_703_a_ACO -1*J_704_a_IDH1 -1*J_705_a_IDH2 -1*J_715_a_CIC1 -(0.0) =< 0.0,		% isoam
		-1*J_706_a_IDH3 +1*J_715_a_CIC1 -(-0.0) >= 0.0, -1*J_706_a_IDH3 +1*J_715_a_CIC1 -(0.0) =< 0.0,		% isoac
		1*J_602_n_CS -1*J_603_n_ACO -1*J_616_n_CIC2TCC -(-0.0) >= 0.0, 1*J_602_n_CS -1*J_603_n_ACO -1*J_616_n_CIC2TCC -(0.0) =< 0.0,		% citnm
		1*J_702_a_CS -1*J_703_a_ACO -1*J_716_a_CIC2TCC -(-0.0) >= 0.0, 1*J_702_a_CS -1*J_703_a_ACO -1*J_716_a_CIC2TCC -(0.0) =< 0.0,		% citam
		-1*J_601_n_PDH +1*J_602_n_CS -1*J_607_n_AKGDH +1*J_608_n_SCS -1*J_617_n_ACL -(-0.0) >= 0.0, -1*J_601_n_PDH +1*J_602_n_CS -1*J_607_n_AKGDH +1*J_608_n_SCS -1*J_617_n_ACL -(0.0) =< 0.0,		% coan
		1*J_616_n_CIC2TCC -1*J_617_n_ACL -(-0.0) >= 0.0, 1*J_616_n_CIC2TCC -1*J_617_n_ACL -(0.0) =< 0.0,		% citnc
		1*J_601_n_PDH -1*J_602_n_CS +1*J_617_n_ACL -(-0.0) >= 0.0, 1*J_601_n_PDH -1*J_602_n_CS +1*J_617_n_ACL -(0.0) =< 0.0,		% acoan
		-1*J_701_a_PDH +1*J_702_a_CS -1*J_707_a_AKGDH +1*J_708_a_SCS -1*J_717_a_ACL -(-0.0) >= 0.0, -1*J_701_a_PDH +1*J_702_a_CS -1*J_707_a_AKGDH +1*J_708_a_SCS -1*J_717_a_ACL -(0.0) =< 0.0,		% coaa
		1*J_716_a_CIC2TCC -1*J_717_a_ACL -(-0.0) >= 0.0, 1*J_716_a_CIC2TCC -1*J_717_a_ACL -(0.0) =< 0.0,		% citac
		1*J_701_a_PDH -1*J_702_a_CS +1*J_717_a_ACL -(-0.0) >= 0.0, 1*J_701_a_PDH -1*J_702_a_CS +1*J_717_a_ACL -(0.0) =< 0.0,		% acoaa
		1*J_406_n_GAPDH -1*J_411_nc_LDH -1*J_611_nc_MDH -1*J_618_n_G3PS -(-0.0) >= 0.0, 1*J_406_n_GAPDH -1*J_411_nc_LDH -1*J_611_nc_MDH -1*J_618_n_G3PS -(0.0) =< 0.0,		% nadhnc
		-1*J_406_n_GAPDH +1*J_411_nc_LDH +1*J_611_nc_MDH +1*J_618_n_G3PS -(-0.0) >= 0.0, -1*J_406_n_GAPDH +1*J_411_nc_LDH +1*J_611_nc_MDH +1*J_618_n_G3PS -(0.0) =< 0.0,		% nadnc
		1*J_506_a_GAPDH -1*J_511_ac_LDH -1*J_711_ac_MDH -1*J_718_a_G3PS -(-0.0) >= 0.0, 1*J_506_a_GAPDH -1*J_511_ac_LDH -1*J_711_ac_MDH -1*J_718_a_G3PS -(0.0) =< 0.0,		% nadhac
		-1*J_506_a_GAPDH +1*J_511_ac_LDH +1*J_711_ac_MDH +1*J_718_a_G3PS -(-0.0) >= 0.0, -1*J_506_a_GAPDH +1*J_511_ac_LDH +1*J_711_ac_MDH +1*J_718_a_G3PS -(0.0) =< 0.0,		% nadac
		-1*J_412_nc_ME +1*J_611_nc_MDH +1*J_614_n_DCC -1*J_619_n_OGC -(-0.0) >= 0.0, -1*J_412_nc_ME +1*J_611_nc_MDH +1*J_614_n_DCC -1*J_619_n_OGC -(0.0) =< 0.0,		% malnc
		-1*J_413_nm_ME +1*J_610_n_FUM -1*J_612_nm_MDH -1*J_614_n_DCC +1*J_619_n_OGC -(-0.0) >= 0.0, -1*J_413_nm_ME +1*J_610_n_FUM -1*J_612_nm_MDH -1*J_614_n_DCC +1*J_619_n_OGC -(0.0) =< 0.0,		% malnm
		-1*J_512_ac_ME +1*J_711_ac_MDH +1*J_714_a_DCC -1*J_719_a_OGC -(-0.0) >= 0.0, -1*J_512_ac_ME +1*J_711_ac_MDH +1*J_714_a_DCC -1*J_719_a_OGC -(0.0) =< 0.0,		% malac
		-1*J_513_am_ME +1*J_710_a_FUM -1*J_712_am_MDH -1*J_714_a_DCC +1*J_719_a_OGC -(-0.0) >= 0.0, -1*J_513_am_ME +1*J_710_a_FUM -1*J_712_am_MDH -1*J_714_a_DCC +1*J_719_a_OGC -(0.0) =< 0.0,		% malam
		1*J_606_n_IDH3 +1*J_619_n_OGC -1*J_621_nc_AAT -(-0.0) >= 0.0, 1*J_606_n_IDH3 +1*J_619_n_OGC -1*J_621_nc_AAT -(0.0) =< 0.0,		% akgnc
		1*J_620_n_AGC -1*J_621_nc_AAT -(-0.0) >= 0.0, 1*J_620_n_AGC -1*J_621_nc_AAT -(0.0) =< 0.0,		% aspnc
		-1*J_103_n_VGLUT +1*J_111_n_PAG +1*J_613_n_GC -1*J_620_n_AGC +1*J_621_nc_AAT -(-0.0) >= 0.0, -1*J_103_n_VGLUT +1*J_111_n_PAG +1*J_613_n_GC -1*J_620_n_AGC +1*J_621_nc_AAT -(0.0) =< 0.0,		% glunc
		-1*J_611_nc_MDH +1*J_617_n_ACL +1*J_621_nc_AAT -(-0.0) >= 0.0, -1*J_611_nc_MDH +1*J_617_n_ACL +1*J_621_nc_AAT -(0.0) =< 0.0,		% oaanc
		-1*J_620_n_AGC -1*J_622_nm_AAT -(-0.0) >= 0.0, -1*J_620_n_AGC -1*J_622_nm_AAT -(0.0) =< 0.0,		% aspnm
		-1*J_602_n_CS +1*J_612_nm_MDH +1*J_622_nm_AAT -(-0.0) >= 0.0, -1*J_602_n_CS +1*J_612_nm_MDH +1*J_622_nm_AAT -(0.0) =< 0.0,		% oaanm
		1*J_706_a_IDH3 +1*J_719_a_OGC -1*J_721_ac_AAT -(-0.0) >= 0.0, 1*J_706_a_IDH3 +1*J_719_a_OGC -1*J_721_ac_AAT -(0.0) =< 0.0,		% akgac
		1*J_720_a_AGC -1*J_721_ac_AAT -(-0.0) >= 0.0, 1*J_720_a_AGC -1*J_721_ac_AAT -(0.0) =< 0.0,		% aspac
		1*J_102_ea_EAAT -1*J_108_a_GS +1*J_713_a_GC -1*J_720_a_AGC +1*J_721_ac_AAT -(-0.0) >= 0.0, 1*J_102_ea_EAAT -1*J_108_a_GS +1*J_713_a_GC -1*J_720_a_AGC +1*J_721_ac_AAT -(0.0) =< 0.0,		% gluac
		-1*J_711_ac_MDH +1*J_717_a_ACL +1*J_721_ac_AAT -(-0.0) >= 0.0, -1*J_711_ac_MDH +1*J_717_a_ACL +1*J_721_ac_AAT -(0.0) =< 0.0,		% oaaac
		-1*J_720_a_AGC -1*J_722_am_AAT -(-0.0) >= 0.0, -1*J_720_a_AGC -1*J_722_am_AAT -(0.0) =< 0.0,		% aspam
		1*J_514_a_PC -1*J_702_a_CS +1*J_712_am_MDH +1*J_722_am_AAT -(-0.0) >= 0.0, 1*J_514_a_PC -1*J_702_a_CS +1*J_712_am_MDH +1*J_722_am_AAT -(0.0) =< 0.0,		% oaaam
		1*J_111_n_PAG +1*J_623_n_GDH -(-0.0) >= 0.0, 1*J_111_n_PAG +1*J_623_n_GDH -(0.0) =< 0.0,		% nh4n
		-1*J_108_a_GS +1*J_723_a_GDH -(-0.0) >= 0.0, -1*J_108_a_GS +1*J_723_a_GDH -(0.0) =< 0.0,		% nh4a
		1*J_413_nm_ME +1*J_600_nm_MCT -1*J_601_n_PDH -1*J_624_n_ALAT -(-0.0) >= 0.0, 1*J_413_nm_ME +1*J_600_nm_MCT -1*J_601_n_PDH -1*J_624_n_ALAT -(0.0) =< 0.0,		% pyrnm
		1*J_513_am_ME -1*J_514_a_PC +1*J_700_am_MCT -1*J_701_a_PDH -1*J_724_a_ALAT -(-0.0) >= 0.0, 1*J_513_am_ME -1*J_514_a_PC +1*J_700_am_MCT -1*J_701_a_PDH -1*J_724_a_ALAT -(0.0) =< 0.0,		% pyram
		1*J_624_n_ALAT -1*J_625_ne_ALA -(-0.0) >= 0.0, 1*J_624_n_ALAT -1*J_625_ne_ALA -(0.0) =< 0.0,		% alan
		1*J_724_a_ALAT -1*J_725_ae_ALA -(-0.0) >= 0.0, 1*J_724_a_ALAT -1*J_725_ae_ALA -(0.0) =< 0.0,		% alaa
		1*J_625_ne_ALA +1*J_725_ae_ALA -(-0.0) >= 0.0, 1*J_625_ne_ALA +1*J_725_ae_ALA -(0.0) =< 0.0,		% alae
		-1*J_613_n_GC +1*J_620_n_AGC +1*J_622_nm_AAT -1*J_623_n_GDH -1*J_624_n_ALAT -1*J_626_n_BCAT -(-0.0) >= 0.0, -1*J_613_n_GC +1*J_620_n_AGC +1*J_622_nm_AAT -1*J_623_n_GDH -1*J_624_n_ALAT -1*J_626_n_BCAT -(0.0) =< 0.0,		% glunm
		1*J_604_n_IDH1 +1*J_605_n_IDH2 -1*J_607_n_AKGDH -1*J_619_n_OGC -1*J_622_nm_AAT +1*J_623_n_GDH +1*J_624_n_ALAT +1*J_626_n_BCAT -(-0.0) >= 0.0, 1*J_604_n_IDH1 +1*J_605_n_IDH2 -1*J_607_n_AKGDH -1*J_619_n_OGC -1*J_622_nm_AAT +1*J_623_n_GDH +1*J_624_n_ALAT +1*J_626_n_BCAT -(0.0) =< 0.0,		% akgnm
		-1*J_713_a_GC +1*J_720_a_AGC +1*J_722_am_AAT -1*J_723_a_GDH -1*J_724_a_ALAT -1*J_726_a_BCAT -(-0.0) >= 0.0, -1*J_713_a_GC +1*J_720_a_AGC +1*J_722_am_AAT -1*J_723_a_GDH -1*J_724_a_ALAT -1*J_726_a_BCAT -(0.0) =< 0.0,		% gluam
		1*J_704_a_IDH1 +1*J_705_a_IDH2 -1*J_707_a_AKGDH -1*J_719_a_OGC -1*J_722_am_AAT +1*J_723_a_GDH +1*J_724_a_ALAT +1*J_726_a_BCAT -(-0.0) >= 0.0, 1*J_704_a_IDH1 +1*J_705_a_IDH2 -1*J_707_a_AKGDH -1*J_719_a_OGC -1*J_722_am_AAT +1*J_723_a_GDH +1*J_724_a_ALAT +1*J_726_a_BCAT -(0.0) =< 0.0,		% akgam
		-1*J_626_n_BCAT -1*J_627_ne_BCKA -(-0.0) >= 0.0, -1*J_626_n_BCAT -1*J_627_ne_BCKA -(0.0) =< 0.0,		% bckan
		1*J_626_n_BCAT -1*J_628_ne_BCAA -(-0.0) >= 0.0, 1*J_626_n_BCAT -1*J_628_ne_BCAA -(0.0) =< 0.0,		% bcaan
		-1*J_726_a_BCAT -1*J_727_ae_BCKA -(-0.0) >= 0.0, -1*J_726_a_BCAT -1*J_727_ae_BCKA -(0.0) =< 0.0,		% bckaa
		1*J_627_ne_BCKA +1*J_727_ae_BCKA -(-0.0) >= 0.0, 1*J_627_ne_BCKA +1*J_727_ae_BCKA -(0.0) =< 0.0,		% bckae
		1*J_726_a_BCAT -1*J_728_ae_BCAA -(-0.0) >= 0.0, 1*J_726_a_BCAT -1*J_728_ae_BCAA -(0.0) =< 0.0,		% bcaaa
		1*J_628_ne_BCAA +1*J_728_ae_BCAA -(-0.0) >= 0.0, 1*J_628_ne_BCAA +1*J_728_ae_BCAA -(0.0) =< 0.0,		% bcaae
		1*J_205_bn_O2 -1*J_800_n_OP -(-0.0) >= 0.0, 1*J_205_bn_O2 -1*J_800_n_OP -(0.0) =< 0.0,		% o2n
		1*J_601_n_PDH +1*J_604_n_IDH1 +1*J_607_n_AKGDH +0.6666666666666666*J_609_n_SDH +1*J_612_nm_MDH +0.6666666666666666*J_618_n_G3PS +1*J_623_n_GDH -2*J_800_n_OP -(-0.0) >= 0.0, 1*J_601_n_PDH +1*J_604_n_IDH1 +1*J_607_n_AKGDH +0.6666666666666666*J_609_n_SDH +1*J_612_nm_MDH +0.6666666666666666*J_618_n_G3PS +1*J_623_n_GDH -2*J_800_n_OP -(0.0) =< 0.0,		% nadhnm
		-1*J_601_n_PDH -1*J_604_n_IDH1 -1*J_607_n_AKGDH -0.6666666666666666*J_609_n_SDH -1*J_612_nm_MDH -0.6666666666666666*J_618_n_G3PS -1*J_623_n_GDH +2*J_800_n_OP -(-0.0) >= 0.0, -1*J_601_n_PDH -1*J_604_n_IDH1 -1*J_607_n_AKGDH -0.6666666666666666*J_609_n_SDH -1*J_612_nm_MDH -0.6666666666666666*J_618_n_G3PS -1*J_623_n_GDH +2*J_800_n_OP -(0.0) =< 0.0,		% nadnm
		1*J_206_ba_O2 -1*J_900_a_OP -(-0.0) >= 0.0, 1*J_206_ba_O2 -1*J_900_a_OP -(0.0) =< 0.0,		% o2a
		1*J_701_a_PDH +1*J_704_a_IDH1 +1*J_707_a_AKGDH +0.6666666666666666*J_709_a_SDH +1*J_712_am_MDH +0.6666666666666666*J_718_a_G3PS +1*J_723_a_GDH -2*J_900_a_OP -(-0.0) >= 0.0, 1*J_701_a_PDH +1*J_704_a_IDH1 +1*J_707_a_AKGDH +0.6666666666666666*J_709_a_SDH +1*J_712_am_MDH +0.6666666666666666*J_718_a_G3PS +1*J_723_a_GDH -2*J_900_a_OP -(0.0) =< 0.0,		% nadham
		-1*J_701_a_PDH -1*J_704_a_IDH1 -1*J_707_a_AKGDH -0.6666666666666666*J_709_a_SDH -1*J_712_am_MDH -0.6666666666666666*J_718_a_G3PS -1*J_723_a_GDH +2*J_900_a_OP -(-0.0) >= 0.0, -1*J_701_a_PDH -1*J_704_a_IDH1 -1*J_707_a_AKGDH -0.6666666666666666*J_709_a_SDH -1*J_712_am_MDH -0.6666666666666666*J_718_a_G3PS -1*J_723_a_GDH +2*J_900_a_OP -(0.0) =< 0.0,		% nadam
		-1.5*J_103_n_VGLUT -1*J_104_n_NKA -1*J_401_n_HK -1*J_403_n_PFK +1*J_407_n_PGK +1*J_410_n_PK +1*J_608_n_SCS -1*J_617_n_ACL +5*J_800_n_OP -1*J_801_n_ATPase -(-0.0) >= 0.0, -1.5*J_103_n_VGLUT -1*J_104_n_NKA -1*J_401_n_HK -1*J_403_n_PFK +1*J_407_n_PGK +1*J_410_n_PK +1*J_608_n_SCS -1*J_617_n_ACL +5*J_800_n_OP -1*J_801_n_ATPase -(0.0) =< 0.0,		% atpn
		1.5*J_103_n_VGLUT +1*J_104_n_NKA +1*J_401_n_HK +1*J_403_n_PFK -1*J_407_n_PGK -1*J_410_n_PK -1*J_608_n_SCS +1*J_617_n_ACL -5*J_800_n_OP +1*J_801_n_ATPase -(-0.0) >= 0.0, 1.5*J_103_n_VGLUT +1*J_104_n_NKA +1*J_401_n_HK +1*J_403_n_PFK -1*J_407_n_PGK -1*J_410_n_PK -1*J_608_n_SCS +1*J_617_n_ACL -5*J_800_n_OP +1*J_801_n_ATPase -(0.0) =< 0.0,		% adpn
		-1*J_105_a_NKA -1*J_108_a_GS -1*J_501_a_HK -1*J_503_a_PFK +1*J_507_a_PGK +1*J_510_a_PK -1*J_514_a_PC +1*J_708_a_SCS -1*J_717_a_ACL +5*J_900_a_OP -1*J_901_a_ATPase -(-0.0) >= 0.0, -1*J_105_a_NKA -1*J_108_a_GS -1*J_501_a_HK -1*J_503_a_PFK +1*J_507_a_PGK +1*J_510_a_PK -1*J_514_a_PC +1*J_708_a_SCS -1*J_717_a_ACL +5*J_900_a_OP -1*J_901_a_ATPase -(0.0) =< 0.0,		% atpa
		1*J_105_a_NKA +1*J_108_a_GS +1*J_501_a_HK +1*J_503_a_PFK -1*J_507_a_PGK -1*J_510_a_PK +1*J_514_a_PC -1*J_708_a_SCS +1*J_717_a_ACL -5*J_900_a_OP +1*J_901_a_ATPase -(-0.0) >= 0.0, 1*J_105_a_NKA +1*J_108_a_GS +1*J_501_a_HK +1*J_503_a_PFK -1*J_507_a_PGK -1*J_510_a_PK +1*J_514_a_PC -1*J_708_a_SCS +1*J_717_a_ACL -5*J_900_a_OP +1*J_901_a_ATPase -(0.0) =< 0.0,		% adpa
		1*J_401_n_HK -1*J_402_n_PGI -3*J_802_n_PPP -(-0.0) >= 0.0, 1*J_401_n_HK -1*J_402_n_PGI -3*J_802_n_PPP -(0.0) =< 0.0,		% g6pn
		-1*J_207_nb_CO2 +1*J_412_nc_ME +1*J_413_nm_ME +1*J_601_n_PDH +1*J_604_n_IDH1 +1*J_605_n_IDH2 +1*J_606_n_IDH3 +1*J_607_n_AKGDH +3*J_802_n_PPP -(-0.0) >= 0.0, -1*J_207_nb_CO2 +1*J_412_nc_ME +1*J_413_nm_ME +1*J_601_n_PDH +1*J_604_n_IDH1 +1*J_605_n_IDH2 +1*J_606_n_IDH3 +1*J_607_n_AKGDH +3*J_802_n_PPP -(0.0) =< 0.0,		% co2n
		1*J_402_n_PGI -1*J_403_n_PFK +2*J_802_n_PPP -(-0.0) >= 0.0, 1*J_402_n_PGI -1*J_403_n_PFK +2*J_802_n_PPP -(0.0) =< 0.0,		% f6pn
		1*J_404_n_ALD +1*J_405_n_TPI -1*J_406_n_GAPDH +1*J_802_n_PPP -(-0.0) >= 0.0, 1*J_404_n_ALD +1*J_405_n_TPI -1*J_406_n_GAPDH +1*J_802_n_PPP -(0.0) =< 0.0,		% gapn
		1*J_501_a_HK -1*J_502_a_PGI -3*J_902_a_PPP -(-0.0) >= 0.0, 1*J_501_a_HK -1*J_502_a_PGI -3*J_902_a_PPP -(0.0) =< 0.0,		% g6pa
		-1*J_208_ab_CO2 +1*J_512_ac_ME +1*J_513_am_ME -1*J_514_a_PC +1*J_701_a_PDH +1*J_704_a_IDH1 +1*J_705_a_IDH2 +1*J_706_a_IDH3 +1*J_707_a_AKGDH +3*J_902_a_PPP -(-0.0) >= 0.0, -1*J_208_ab_CO2 +1*J_512_ac_ME +1*J_513_am_ME -1*J_514_a_PC +1*J_701_a_PDH +1*J_704_a_IDH1 +1*J_705_a_IDH2 +1*J_706_a_IDH3 +1*J_707_a_AKGDH +3*J_902_a_PPP -(0.0) =< 0.0,		% co2a
		1*J_502_a_PGI -1*J_503_a_PFK +2*J_902_a_PPP -(-0.0) >= 0.0, 1*J_502_a_PGI -1*J_503_a_PFK +2*J_902_a_PPP -(0.0) =< 0.0,		% f6pa
		1*J_504_a_ALD +1*J_505_a_TPI -1*J_506_a_GAPDH +1*J_902_a_PPP -(-0.0) >= 0.0, 1*J_504_a_ALD +1*J_505_a_TPI -1*J_506_a_GAPDH +1*J_902_a_PPP -(0.0) =< 0.0,		% gapa
		1*J_412_nc_ME +1*J_606_n_IDH3 +6*J_802_n_PPP -1*J_803_n_GR1 -(-0.0) >= 0.0, 1*J_412_nc_ME +1*J_606_n_IDH3 +6*J_802_n_PPP -1*J_803_n_GR1 -(0.0) =< 0.0,		% nadphn
		-1*J_412_nc_ME -1*J_606_n_IDH3 -6*J_802_n_PPP +1*J_803_n_GR1 -(-0.0) >= 0.0, -1*J_412_nc_ME -1*J_606_n_IDH3 -6*J_802_n_PPP +1*J_803_n_GR1 -(0.0) =< 0.0,		% nadpn
		1*J_413_nm_ME +1*J_605_n_IDH2 -1*J_804_n_GR2 -(-0.0) >= 0.0, 1*J_413_nm_ME +1*J_605_n_IDH2 -1*J_804_n_GR2 -(0.0) =< 0.0,		% nadphnm
		-1*J_413_nm_ME -1*J_605_n_IDH2 +1*J_804_n_GR2 -(-0.0) >= 0.0, -1*J_413_nm_ME -1*J_605_n_IDH2 +1*J_804_n_GR2 -(0.0) =< 0.0,		% nadpnm
		0.025*J_800_n_OP -1*J_805_n_GPX -(-0.0) >= 0.0, 0.025*J_800_n_OP -1*J_805_n_GPX -(0.0) =< 0.0,		% rosn
		2*J_803_n_GR1 +2*J_804_n_GR2 -2*J_805_n_GPX -(-0.0) >= 0.0, 2*J_803_n_GR1 +2*J_804_n_GR2 -2*J_805_n_GPX -(0.0) =< 0.0,		% gshn
		-1*J_803_n_GR1 -1*J_804_n_GR2 +1*J_805_n_GPX -(-0.0) >= 0.0, -1*J_803_n_GR1 -1*J_804_n_GR2 +1*J_805_n_GPX -(0.0) =< 0.0,		% gssgn
		1*J_512_ac_ME +1*J_706_a_IDH3 +6*J_902_a_PPP -1*J_903_a_GR1 -(-0.0) >= 0.0, 1*J_512_ac_ME +1*J_706_a_IDH3 +6*J_902_a_PPP -1*J_903_a_GR1 -(0.0) =< 0.0,		% nadpha
		-1*J_512_ac_ME -1*J_706_a_IDH3 -6*J_902_a_PPP +1*J_903_a_GR1 -(-0.0) >= 0.0, -1*J_512_ac_ME -1*J_706_a_IDH3 -6*J_902_a_PPP +1*J_903_a_GR1 -(0.0) =< 0.0,		% nadpa
		1*J_513_am_ME +1*J_705_a_IDH2 -1*J_904_a_GR2 -(-0.0) >= 0.0, 1*J_513_am_ME +1*J_705_a_IDH2 -1*J_904_a_GR2 -(0.0) =< 0.0,		% nadpham
		-1*J_513_am_ME -1*J_705_a_IDH2 +1*J_904_a_GR2 -(-0.0) >= 0.0, -1*J_513_am_ME -1*J_705_a_IDH2 +1*J_904_a_GR2 -(0.0) =< 0.0,		% nadpam
		0.25*J_900_a_OP -1*J_905_a_GPX -(-0.0) >= 0.0, 0.25*J_900_a_OP -1*J_905_a_GPX -(0.0) =< 0.0,		% rosa
		2*J_903_a_GR1 +2*J_904_a_GR2 -2*J_905_a_GPX -(-0.0) >= 0.0, 2*J_903_a_GR1 +2*J_904_a_GR2 -2*J_905_a_GPX -(0.0) =< 0.0,		% gsha
		-1*J_903_a_GR1 -1*J_904_a_GR2 +1*J_905_a_GPX -(-0.0) >= 0.0, -1*J_903_a_GR1 -1*J_904_a_GR2 +1*J_905_a_GPX -(0.0) =< 0.0,		% gssga
Vcyc = J_109_ae_SN,
Vcyc < 0.51,
J_201_be_GLUT > 0,
J_801_n_ATPase > 2,
J_801_n_ATPase < 4,
J_901_a_ATPase > 1,
J_901_a_ATPase < 2,
		0 = 0
	},
	true.
