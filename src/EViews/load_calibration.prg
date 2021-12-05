' This subroutine will be deleted when the calibration program is active

subroutine load_calibration

  ' Load parameter from Excel and store them into a matrix (L,C)
 
  matrix(60,9) BUILDING                                  ' Matrix of the BUILDING block data
  BUILDING.read(D5,s=BUILDING)  .\..\..\data\France\DATA_BUILDING.xls

  matrix(80,9) TRANSPORT                                  ' Matrix of the TRANSPORT block data
  TRANSPORT.read(D5,s=TRANSPORT)  .\..\..\data\France\DATA_TRANSPORT.xls

  matrix(32,6) ELAS                                  ' Matrix of the ELASTICTIES block data
ELAS.read(B3,s = ELAS)  .\..\..\data\France\DATA_ELAS.xls

  matrix(32,6) ELAS_NRJ                                  ' Matrix of the ENERGY ELASTICTIES block data
ELAS_NRJ.read(B3,s = ELAS_NRJ)  .\..\..\data\France\DATA_ELAS_NRJ.xls

endsub
