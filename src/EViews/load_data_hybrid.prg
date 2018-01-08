' ***********************************************************************************************************************
' ******************************************* HOUSEHOLDS HYBRID****************************************************************************
' ***********************************************************************************************************************8


subroutine load_calibration_hybrid

  matrix(300,10) HOUSEHOLD_HYBRID_BUIL                          ' Matrix of the data HOUSEHOLD_HYBRID_BUIL
  HOUSEHOLD_HYBRID_BUIL.read(D4,s=Household_hybrid_BUIL) {%data_calibration}

  matrix(280,10) HOUSEHOLD_HYBRID_AUTO                          ' Matrix of the data HOUSEHOLD_HYBRID_AUTO
  HOUSEHOLD_HYBRID_AUTO.read(D4,s=Household_hybrid_AUTO) {%data_calibration}

  matrix(100,10) HOUSEHOLD_HYBRID_Trans                         ' Matrix of the data HOUSEHOLD_HYBRID_Trans
  HOUSEHOLD_HYBRID_Trans.read(D4,s=Household_hybrid_Transition) {%data_calibration}

  matrix(23,50) MTEP                     ' Matrix of the energy production features
  MTEP.read(C4,s=Donnees_energie) {%data_calibration}

  matrix(35,5) MTEP_USE                   ' Matrix of the energy production features
  MTEP_USE.read(C33,s=Donnees_energie) {%data_calibration}

  matrix(37,4) ENER_INDUS                     ' Matrix of the energy production features
  ENER_INDUS.read(C5,s=EnergyIndus) {%data_calibration}

  matrix(10,6) ES_BUILNRJ                         ' Matrix of elasticity of substitution ( between type of energy for building)
  ES_BUILNRJ.read(B4,s=ELAS_Hybrid_BUILNRJ) {%data_calibration}

  matrix(4,4) MTEP_SHARE                     ' Matrix of final energy consumption share between households and sectors, and within households, by energy type
  MTEP_SHARE.read(C39,s=Donnees_energie) {%data_calibration}

  matrix(20,4) MTEP_SECTOR_SHARE                     ' Matrix of final energy consumption by sector and energy carrier
  MTEP_SECTOR_SHARE.read(C68,s=Donnees_energie) {%data_calibration}

endsub