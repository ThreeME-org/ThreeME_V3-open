' This file contains E-Views subroutines:
' 1. create_lists: create the lists to be read by E-Views
'     - for the construction of series

subroutine create_lists

  %list_sec = "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406" '[s]
  %list_sec_Market = "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406"
  %list_sec_E = "21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406" '[se]

  %list_com = "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24" '[c]
  %list_com_MAT = "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20" '[cm]
  %list_com_E = "21 22 23 24" '[ce]
  %list_com_E_CO2 = "21 22 24" '[ce2]

  %list_trsp = "14 15 16 17 18" '[trsp]
  %list_trsp_travel = "14 15 18"

  %list_com_oth =  "01 02 04 05 06 07 08 09 10 11 12 16 17 19 20" '[co]


  %list_age = "15 20 25 55 60 65" '[age]
  %list_sex = "W M" '[sex]


  %list_household = "H01" '' H02 H03 H04 H05" '[h]
  if %list_household="H01" then
    !step_HH = 0
  else
    !step_HH = 1
  endif

  %list_ener_class = "cA cB cC cD cE cF cG" '[ecl]
  %list_buil_class = "CA CB CC CD CE CF CG DES" '[bcl]




  ' Matrix of column numbers of elasticities of substitution
  ' for transport margins
  ' +----+----+----+----+----+
  ' |    | 14 | 16 | 17 | 18 |
  ' +----+----+----+----+----+
  ' | 14 |    | 1  | 2  | 3  |
  ' +----+----+----+----+----+
  ' | 16 | 1  |    | 4  | 5  |
  ' +----+----+----+----+----+
  ' | 17 | 2  | 4  |    | 6  |
  ' +----+----+----+----+----+
  ' | 18 | 3  | 5  | 6  |    |
  ' +----+----+----+----+----+

  matrix(18, 18) cols_trsp
  cols_trsp.fill(o = 250) 1, 2, 3
  cols_trsp.fill(o = 284) 1, 0, 0, 4, 5
  cols_trsp.fill(o = 302) 2, 0, 4, 0, 6
  cols_trsp.fill(o = 320) 3, 0, 5, 6

  ' Matrix of column numbers of elasticities of substitution
  ' for energy consumptions
  ' +----+----+----+----+----+
  ' |    | 21 | 22 | 23 | 24 |
  ' +----+----+----+----+----+
  ' | 21 |    | 1  | 2  | 3  |
  ' +----+----+----+----+----+
  ' | 22 | 1  |    | 4  | 5  |
  ' +----+----+----+----+----+
  ' | 23 | 2  | 4  |    | 6  |
  ' +----+----+----+----+----+
  ' | 24 | 3  | 5  | 6  |    |
  ' +----+----+----+----+----+
  matrix(24, 24) cols_ce
  cols_ce.fill(o = 502) 1, 2, 3
  cols_ce.fill(o = 525) 1, 0, 4, 5
  cols_ce.fill(o = 549) 2, 4, 0, 6
  cols_ce.fill(o = 573) 3, 5, 6

  ' Matrix of column numbers of elasticities of substitution
  ' for transport of intermediary consumptions
  ' +----+----+----+----+----+----+
  ' |    | 14 | 15 | 16 | 17 | 18 |
  ' +----+----+----+----+----+----+
  ' | 14 |    | 1  | 2  | 3  | 4  |
  ' +----+----+----+----+----+----+
  ' | 15 | 1  |    | 1  | 5  | 6  |
  ' +----+----+----+----+----+----+
  ' | 16 | 2  | 1  |    | 7  | 8  |
  ' +----+----+----+----+----+----+
  ' | 17 | 3  | 5  | 7  |    | 9  |
  ' +----+----+----+----+----+----+
  ' | 18 | 4  | 6  | 8  | 9  |    |
  ' +----+----+----+----+----+----+
  ' HACK: transport sectors 15 and 16 can't be substituted to each other
  ' the pair is disabled manually in the susbtitution equation
  matrix(18, 18) cols_mat
  cols_mat.fill(o = 249) 1, 2, 3, 4
  cols_mat.fill(o = 266) 1, 0, 1, 5, 6
  cols_mat.fill(o = 284) 2, 1, 0, 7, 8
  cols_mat.fill(o = 302) 3, 5, 7, 0, 9
  cols_mat.fill(o = 320) 4, 6, 8, 9

endsub
