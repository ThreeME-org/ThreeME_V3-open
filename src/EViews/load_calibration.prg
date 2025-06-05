' This subroutine will be deleted when the calibration program is active

subroutine load_calibration

  ' Load parameter from Excel and store them into a matrix (L,C)

  
  matrix(60,8) BUILDING                                  ' Matrix of the BUILDING block data
  BUILDING.read(D5,s=BUILDING)          {%data_calibration}

  if @neqna(%shocks, "euro") then   ' WHY THIS conditionnality???? 
    matrix(60,8) BUILDING                                  ' Matrix of the hybrid block data
    BUILDING.read(D5,s=BUILDING)          {%data_calibration}
  endif

  matrix(80,9) TRANSPORT                                  ' Matrix of the TRANSPORT block data
  TRANSPORT.read(D5,s=TRANSPORT)          {%data_calibration}



endsub
