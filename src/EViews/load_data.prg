' ============================================================================ 
' Series of subroutines to load data
' ============================================================================ 

' ============================================================================ 
' ++++++++++++++++++++
' Subroutine "load_excel"
' ++++++++++++++++++++

' Loads the time series data that defines the baseline scenario or the shock
' The data will overwrite the default trajectorty of the baseline-steady state scenario from the baseyear
' The subroutine has 3 arguments: "directy name", "file name", "sheet name"
' The the directory should be located in ...\ThreeME_V3\src\data

subroutine load_excel(string %directory, string %file, string %sheet)
  
  smpl {%baseyear} @last              ' Load from the baseyear
  vector(1) vectnb                    ' Create a vector with 1 row
  vectnb.read(a1,s={%sheet}) .\..\..\data\{%directory}\{%file}.xls 1       ' Load the number of series in the vector
  !seriesnb=vectnb(1)                 ' Load the number of series as a parameter

  ' Load the historical data from Excel Inputs : Cell_number; Format omitted(t=xls); s = Sheet_name; "t" : transpose (read in row); File_name; number of series
  read(c2,s={%sheet},t) .\..\..\data\{%directory}\{%file}.xls !seriesnb
  smpl @all

endsub


' ============================================================================ 
' +++++++++++++++++++
' Subroutine "load_realist"
' +++++++++++++++++++

' Loads the data defining the realistic baseline scenario

subroutine load_realist()

  vector(1) vectnb_4

  vectnb_4.read(a1,s=baseline-realistic) {%data_calibration} 1
  !exo_realistic=vectnb_4(1)
  read(c2,s=exo_realistic_1,t) {%data_calibration} !exo_realistic


endsub

' ============================================================================ 
' +++++++++++++++++++
' Subroutine "load_data_shocks"
' +++++++++++++++++++

' Loads the data defining the shock applied in the currently run scenario

subroutine load_data_shocks(string %data_shocks)

  smpl @all
  vector(1) vectnb                    ' Create a vector with 1 row
  vectnb.read(a1,s=series) .\..\..\data\shocks\{%data_shocks}.xls 1       ' Load the number of series in the vector
  !seriesnb=vectnb(1)                 ' Load the number of series in a parameter

  ' Load the historical data from Excel Inputs : Cell_number; Format omitted(t=xls); s = Sheet_naMe; "t" : transpose (read in row); File_name; number of series
  read(c2,s=series,t) .\..\..\data\shocks\{%data_shocks}.xls !seriesnb

endsub


' ============================================================================ 
' +++++++++++++++++++
' Subroutine "load_xl"
' +++++++++++++++++++

' Loads the data defining the shock applied in the currently run scenario

subroutine load_xl(string %file, string %sheet)

  smpl @all
  vector(1) vectnb                    ' Create a vector with 1 row
  vectnb.read(a1,s={%sheet}) .\..\..\data\shocks\{%file}.xls 1       ' Load the number of series in the vector
  !seriesnb=vectnb(1)                 ' Load the nuMber of series in a paraMeter

  ' Load the historical data froM Excel Inputs : Cell_nuMber; ForMat omitted(t=xls); s = Sheet_naMe; "t" : transpose (read in roW); File_naMe; nuMber of series
  read(c2,s={%sheet},t) .\..\..\data\shocks\{%file}.xls !seriesnb

endsub



