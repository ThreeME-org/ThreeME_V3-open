' Loads the data defining the shock applied in the currently run scenario
subroutine load_data_shocks(string %data_shocks)

  smpl @all
  vector(1) vectnb                    ' Create a vector with 1 row
  vectnb.read(a1,s=series) .\..\..\data\shocks\{%data_shocks}.xls 1       ' Load the number of series in the vector
  !seriesnb=vectnb(1)                 ' Load the number of series in a parameter

  ' Load the historical data from Excel Inputs : Cell_number; Format omitted(t=xls); s = Sheet_naMe; "t" : transpose (read in row); File_name; number of series
  read(c2,s=series,t) .\..\..\data\shocks\{%data_shocks}.xls !seriesnb

endsub


' Loads the data defining the shock applied in the currently run scenario
subroutine load_xl(string %file, string %sheet)
  smpl @all
  vector(1) vectnb                    ' Create a vector with 1 row
  vectnb.read(a1,s={%sheet}) .\..\..\data\shocks\{%file}.xls 1       ' Load the number of series in the vector
  !seriesnb=vectnb(1)                 ' Load the nuMber of series in a paraMeter

  ' Load the historical data froM Excel Inputs : Cell_nuMber; ForMat omitted(t=xls); s = Sheet_naMe; "t" : transpose (read in roW); File_naMe; nuMber of series
  read(c2,s={%sheet},t) .\..\..\data\shocks\{%file}.xls !seriesnb
endsub




subroutine load_shocks(string %scenario_name)
  ' Load data for the shock to be simulated
  call load_data_shocks(".\..\..\data\shocks\" + %scenario_name + ".xls")
endsub
