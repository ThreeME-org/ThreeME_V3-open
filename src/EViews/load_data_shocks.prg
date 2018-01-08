' Loads the data defining the shock applied in the currently run scenario
subroutine load_data_shocks(string %data_shocks)
  smpl @all
  vector(1) vectnb                    ' Create a vector With 1 roW
  vectnb.read(a1,s=series) {%data_shocks} 1       ' Load the nuMber of series in the vector
  !seriesnb=vectnb(1)                 ' Load the nuMber of series in a paraMeter

  ' Load the historical data froM Excel Inputs : Cell_nuMber; ForMat omitted(t=xls); s = Sheet_naMe; "t" : transpose (read in roW); File_naMe; nuMber of series
  read(c2,s=series,t) {%data_shocks} !seriesnb
endsub


' Loads the data defining the shock applied in the currently run scenario
subroutine load_xl(string %file, string %sheet)
  smpl @all
  vector(1) vectnb                    ' Create a vector With 1 roW
  vectnb.read(a1,s={%sheet}) .\..\..\data\shocks\{%file}.xls 1       ' Load the nuMber of series in the vector
  !seriesnb=vectnb(1)                 ' Load the nuMber of series in a paraMeter

  ' Load the historical data froM Excel Inputs : Cell_nuMber; ForMat omitted(t=xls); s = Sheet_naMe; "t" : transpose (read in roW); File_naMe; nuMber of series
  read(c2,s={%sheet},t) .\..\..\data\shocks\{%file}.xls !seriesnb
endsub
