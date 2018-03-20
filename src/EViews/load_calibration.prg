' This subroutine will be deleted when the calibration program is active

subroutine load_calibration

  ' Load parameter from Excel and store them into a matrix (L,C)

  
  matrix(150,1) STEADYSTATE                                     ' Matrix of the coefficients of the Steady state rate
  STEADYSTATE.read(B2,s=BaselineHypotheses) {%data_calibration}

  matrix(40,40) SUT                                     ' Matrix of the SUT data
  SUT.read(E4,s=SUT) {%data_calibration}

  matrix(7,1) HH_INC                                  ' Matrix of the households income data
  HH_INC.read(D3,s=HH_INC)          {%data_calibration}

  matrix(40,25) TAXES                                  ' Matrix of the detailled taxes data
  TAXES.read(D4,s=TAXES)          {%data_calibration}




endsub
