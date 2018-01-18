' This subroutine will be deleted when the calibration program is active

subroutine load_calibration

  ' Load parameter from Excel and store them into a matrix (L,C)

  
  matrix(150,1) STEADYSTATE                                     ' Matrix of the coefficients of the Steady state rate
  STEADYSTATE.read(B2,s=BaselineHypotheses) {%data_calibration}

  matrix(40,40) SUT                                     ' Matrix of the SUT data
  SUT.read(E4,s=SUT) {%data_calibration}

  matrix(37,7) ES_KLEM                                  ' Matrix of elasticity of substitution (level 1, KLEM)
  ES_KLEM.read(B4,s=ELAS_L1_KLEM)          {%data_calibration}




'  matrix(100,6) ADJUST                                      ' Matrix of the coefficients of the ajustment processes
'  ADJUST.read(C4,s=Adjustment) {%data_calibration}
'

'
'
'  matrix(37,6) ES_NRJ                                   ' Matrix of elasticity of substitution (level 2, between type of energy)
'  ES_NRJ.read(B4,s=ELAS_L2_NRJ) {%data_calibration}
'
'  matrix(37,9) ES_TRANSP_CI                                 ' Matrix of elasticity of substitution (level 2, between type of transport)
'  ES_TRANSP_CI.read(B4,s=ELAS_L2_TRANSPORT) {%data_calibration}
'
'  matrix(24,6) ES_TRANSP_MARG                                   ' Matrix of elasticity of substitution (Level 3, Transport Margins, between type of transport)
'  ES_TRANSP_MARG.read(B4,s=ELAS_TRANSP_MARGIN) {%data_calibration}
'
'  matrix(37,24) ES_CIM                                  ' Matrix of elasticity of substitution (level 3, Material between domestic and imported)
'  ES_CIM.read(B4,s=ELAS_L3_IMP_DOM) {%data_calibration}
'
'  matrix(37,24) ES_IAM                                  ' Matrix of elasticity of substitution (level 3, Investment allocation between domestic and imported)
'  ES_IAM.read(B4,s=ELAS_INVEST) {%data_calibration}
'
'


endsub
