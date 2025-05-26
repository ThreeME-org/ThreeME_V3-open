' ============================================================================ 
' Series of subroutines to load data
' ============================================================================ 

' ============================================================================ 
' +++++++++++++++++++
' Subroutine "load_realist"
' +++++++++++++++++++

' Loads the data defining the realistic baseline scenario

subroutine load_realist()

  vector(1) vectnb_4

  vectnb_4.read(a1,s=exo_realistic_1) {%data_calibration} 1
  !exo_realistic=vectnb_4(1)
  read(c2,s=exo_realistic_1,t) {%data_calibration} !exo_realistic


  vectnb_4.read(a1,s=exo_realistic_Hybrid) {%data_calibration} 1
  !exo_realistic=vectnb_4(1)
  read(c2,s=exo_realistic_Hybrid,t) {%data_calibration} !exo_realistic

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



