' This subroutine will be deleted when the calibration program is active

subroutine load_calibration

  ' Load parameter from Excel and store them into a matrix (L,C)

  
  matrix(40,40) SUT                                     ' Matrix of the SUT data
  SUT.read(E4,s=SUT) {%data_calibration}

  matrix(7,1) HH_INC                                  ' Matrix of the households income data
  HH_INC.read(D3,s=HH_INC)          {%data_calibration}

  matrix(40,25) TAXES                                  ' Matrix of the detailled taxes data
  TAXES.read(D4,s=TAXES)          {%data_calibration}

''  matrix(200,1) Hybrid_BUIL                                  ' Matrix of the hybrid block data
''  Hybrid_BUIL.read(E4,s=Hybrid_BUIL)          {%data_calibration}


''  matrix(40,10) Hybrid_TRANS                                  ' Matrix of the hybrid block data
''  Hybrid_TRANS.read(D5,s=Hybrid_TRANSITION)          {%data_calibration}




endsub
