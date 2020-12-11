' This subroutine will be deleted when the calibration program is active

subroutine load_calibration

  ' Load parameter from Excel and store them into a matrix (L,C)
 
  matrix(60,8) BUILDING                                  ' Matrix of the BUILDING block data
  BUILDING.read(D5,s=BUILDING)  .\..\..\data\calibrations\DATA_BUILDING.xls

  matrix(80,9) TRANSPORT                                  ' Matrix of the TRANSPORT block data
  TRANSPORT.read(D5,s=TRANSPORT)  .\..\..\data\calibrations\DATA.xls


endsub
