' This subroutine will be deleted when the calibration program is active

subroutine load_calibration

  ' Load parameter from Excel and store them into a matrix (L,C)

  
  matrix(60,8) BUILDING                                  ' Matrix of the hybrid block data
  BUILDING.read(D5,s=BUILDING)          {%data_calibration}


endsub
