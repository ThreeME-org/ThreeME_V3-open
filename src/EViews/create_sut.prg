' ============================================================================
' ============================================================================
' ==============    CREATE SUT       =========================================
' ============================================================================

' This subroutine creates Supply Use Table (SUT) from simulated scenario and given years

subroutine create_sut(string %years, string %scenarii)

statusline Creating Supply Use Table for year(s) {%years} and scenario(s) {%scenarii}

' Create table for all results (years and scenarii)
table SUTALL

' Initialize coordonates table SUTALL
!rowsutall = 1
for %year {%years} 

' Initialize coordonates table SUTALL
!colsutall = 1
for %scenario {%scenarii} 


  ' Counting the number of elements in the lists
  scalar ncom = @wcount(%list_com)
  scalar nsec = @wcount(%list_sec)
  scalar nmarg = @wcount(%list_com_MARG)

  ' Cell where the SUT matrix starts
  scalar firstrow = 4
  scalar firstcol = 4


  ' Calculate the size of the matrix
  scalar nrow = 6*ncom + 35 + (firstrow-1)
  scalar ncol = 3*nsec + nmarg + 7 + (firstcol-1)


  ' Jump parameters
  !jumpdom = ncom + 12
  !jumpimp = 2*ncom + 14
  !jumpval = 3*ncom + 20

  ' Check the size of the lists and of the matrix 
''  scalar answer = @uiprompt("There are "+@str(ncom)+" commodities, "+@str(nsec)+" sectors and "+@str(nmarg)+" margins commodities. The SUT matrix will have "+@str(nrow-(firstcol-1))+ " rows and "+@str(ncol-(firstcol-1))+" columns. Do you wish to proceed?", "YN")
''    if answer = 2 then
''        return
''    endif

  ' Create table for one year and one scenario
  table(nrow, ncol) tab{%year}S{%scenario}

  ' Create matrix
  ' matrix(nrow, ncol) mat{%year}S{%scenario}
  ' mat{%year}S{%scenario} = na

  ' ****************************************************************
  ' ****** FILL MATRIX HEADERS *************************************
  ' ****************************************************************

  ' *************   Input colum headers

  ' Input the table and matrix column number
  For !i=1 to ncol-(firstcol-1)
  '    mat{%year}S{%scenario}(firstrow-1,!i+firstcol-1) = !i
      tab{%year}S{%scenario}(firstrow-1,!i+firstcol-1) = !i
  next

  !col=firstcol

  ' PRODUCTION headers
  tab{%year}S{%scenario}(firstrow-3,!col) = "PRODUCTION"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "PRODUCTION"

  For %s {%list_sec}
    tab{%year}S{%scenario}(firstrow-2,!col) = %s
    tab{%year}S{%scenario}(firstrow-2 + !jumpval,!col) = %s
  !col=!col+1
  next

  ' IMPORTS header
  tab{%year}S{%scenario}(firstrow-3,!col) = "IMPORTS"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "IMPORTS"
  !col=!col+1

  ' MARGINS PAID headers
  tab{%year}S{%scenario}(firstrow-3,!col) = "MARGINS PAID"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "MARGINS PAID"

  For %m {%list_com_MARG}
    tab{%year}S{%scenario}(firstrow-2,!col) = %m
    tab{%year}S{%scenario}(firstrow-2 + !jumpval,!col) = %m
  !col=!col+1
  next

  ' NET TAX header
  tab{%year}S{%scenario}(firstrow-3,!col) = "TAXES LESS SUBSIDIES"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "TAXES LESS SUBSIDIES"

  !col=!col+1

  ' MARGINS SUPPLIED header
  tab{%year}S{%scenario}(firstrow-3,!col) = "MARGINS SUPPLIED"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "MARGINS SUPPLIED"

  !col=!col+1

  ' INTERMEDIARIES headers
  tab{%year}S{%scenario}(firstrow-3,!col) = "INTERMEDIARY CONSUMPTION"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "INTERMEDIARY CONSUMPTION"

  For %s {%list_sec}
    tab{%year}S{%scenario}(firstrow-2,!col) = %s
    tab{%year}S{%scenario}(firstrow-2 + !jumpval,!col) = %s

  !col=!col+1
  next

  ' FINAL CONSUMPTION headers
  tab{%year}S{%scenario}(firstrow-3,!col) = "FINAL CONSUMPTION"
  tab{%year}S{%scenario}(firstrow-2,!col) = "HOUSEHOLDS"

  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "FINAL CONSUMPTION"
  tab{%year}S{%scenario}(firstrow-2 + !jumpval,!col) = "HOUSEHOLDS"
  !col=!col+1

  tab{%year}S{%scenario}(firstrow-2,!col) = "GOVERNMENT"
  tab{%year}S{%scenario}(firstrow-2,!col) = "GOVERNMENT"

  !col=!col+1

  ' INVESTMENT headers
  tab{%year}S{%scenario}(firstrow-3,!col) = "INVESTMENT"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "INVESTMENT"

  For %s {%list_sec}
    tab{%year}S{%scenario}(firstrow-2,!col) = %s
    tab{%year}S{%scenario}(firstrow-2 + !jumpval,!col) = %s

  !col=!col+1
  next
  
  ' EXPORTS header
  tab{%year}S{%scenario}(firstrow-3,!col) = "EXPORTS"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "EXPORTS"

  !col=!col+1

  ' ECHANGE IN STOCKS header
  tab{%year}S{%scenario}(firstrow-3,!col) = "STOCKS"
  tab{%year}S{%scenario}(firstrow-3 + !jumpval,!col) = "STOCKS"

  !col=!col+1

  ' *********** Input row headers
  ' Input the table and matrix row number
  For !i=1 to nrow-(firstrow-1)
      ' mat{%year}S{%scenario}(!i+firstrow-1,firstcol-1) = !i
      tab{%year}S{%scenario}(!i+firstrow-1,firstcol-1) = !i
  next

  !row=firstrow 
  ' For Volume
  tab{%year}S{%scenario}(!row-3,firstcol-3) = "SCENARIO "+%scenario
  tab{%year}S{%scenario}(!row-2,firstcol-3) = "VOLUME "+%year
  tab{%year}S{%scenario}(!row,firstcol-3) = "TOTAL"
  tab{%year}S{%scenario}(!row+1,firstcol-3) = "COMMODITIES"

  tab{%year}S{%scenario}(!row + !jumpdom,firstcol-3) = "DOMESTIC"
  tab{%year}S{%scenario}(!row + !jumpdom+1,firstcol-3) = "COMMODITIES"

  tab{%year}S{%scenario}(!row + !jumpimp,firstcol-3) = "IMPORTED"
  tab{%year}S{%scenario}(!row + !jumpimp+1,firstcol-3) = "COMMODITIES"

  ' For Value
  tab{%year}S{%scenario}(!row-2 + !jumpval,firstcol-3) = "VALUES "+%year
  tab{%year}S{%scenario}(!row + !jumpval,firstcol-3) = "TOTAL"
  tab{%year}S{%scenario}(!row+1 + !jumpval,firstcol-3) = "COMMODITIES"

  tab{%year}S{%scenario}(!row + !jumpdom + !jumpval,firstcol-3) = "DOMESTIC"
  tab{%year}S{%scenario}(!row + !jumpdom+1 + !jumpval,firstcol-3) = "COMMODITIES"

  tab{%year}S{%scenario}(!row + !jumpimp + !jumpval,firstcol-3) = "IMPORTED"
  tab{%year}S{%scenario}(!row + !jumpimp+1 + !jumpval,firstcol-3) = "COMMODITIES"

  For %c {%list_com}
      ' For total
      tab{%year}S{%scenario}(!row,firstcol-2) = %c
      tab{%year}S{%scenario}(!row + !jumpval,firstcol-2) = %c

      ' For Domestic
      tab{%year}S{%scenario}(!row + !jumpdom,firstcol-2) = %c
      tab{%year}S{%scenario}(!row + !jumpdom + !jumpval,firstcol-2) = %c

      ' For imported
      tab{%year}S{%scenario}(!row + !jumpimp,firstcol-2) = %c
      tab{%year}S{%scenario}(!row + !jumpimp + !jumpval,firstcol-2) = %c

  !row=!row+1
  next

  ' For total
  tab{%year}S{%scenario}(!row,firstcol-2) = "Total"
  tab{%year}S{%scenario}(!row+!jumpval,firstcol-2) = "Total"

  ' For Domestic
  tab{%year}S{%scenario}(!row+!jumpdom,firstcol-2) = "Total"
  tab{%year}S{%scenario}(!row+!jumpdom +!jumpval,firstcol-2) = "Total"

  ' For imported
  tab{%year}S{%scenario}(!row+!jumpimp,firstcol-2) = "Total"
  tab{%year}S{%scenario}(!row+!jumpimp+!jumpval,firstcol-2) = "Total"

  !row=!row+1

  For %header "Value added" "Taxes on sectors" Wages "Employer social contribution" Labor Investment "Depreciation ratio" "Capital stock" "Gross Operating Surplus" "Net Operating Surplus"
      tab{%year}S{%scenario}(!row,firstcol-2) = %header
      tab{%year}S{%scenario}(!row+!jumpval,firstcol-2) = %header

  !row=!row+1
  next

  ' ****************************************************************
  ' ****** FILL UP MATRIX ******************************************
  ' ****************************************************************
  %obj = "tab"

  ' *********** LOAD COMMODITIES DETAIL 
  ' Load matrix of production
  !col=firstcol
  For %s {%list_sec}

    !row=firstrow 
    For %c {%list_com}
      ' {%obj}{%year}S{%scenario}(!row,!col) = !row + !col/100

    if @elem(Y_{%c}_{%s},%baseyear) <> 0 then
'               Equivalent to:    if @isobject("Y_"+%c+"_"+%s+"_"+%scenario)=1 then
'                       or to:    if @isobject("Y_"+%c+"_"+%s+"_"+%scenario) then   ' =1 (exist, can be omited) = 0 (do not exist) 

      ' For total
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(Y_{%c}_{%s}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row+!jumpval,!col) = @elem(PY_{%s}_{%scenario},%year) * @elem(Y_{%c}_{%s}_{%scenario},%year)

      ' For Domestic
      tab{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(Y_{%c}_{%s}_{%scenario},%year)
      tab{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PY_{%s}_{%scenario},%year) * @elem(Y_{%c}_{%s}_{%scenario},%year)


    else
      ' For total    
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0

      ' For Domestic
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0

    endif

    !row=!row+1
    next

    if @elem(Y_{%s},%baseyear) <> 0 then
          ' For total
          {%obj}{%year}S{%scenario}(!row,!col) = @elem(Y_{%s}_{%scenario},%year)
          {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PY_{%s}_{%scenario},%year) * @elem(Y_{%s}_{%scenario},%year)

          ' For Domestic
          {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(Y_{%s}_{%scenario},%year)
          {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) =  @elem(PY_{%s}_{%scenario},%year) * @elem(Y_{%s}_{%scenario},%year)

    else
          ' For total
          {%obj}{%year}S{%scenario}(!row,!col) = 0
          {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0

          ' For Domestic
          {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
          {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0

    endif

    ' Creating missing series for next loop
    series PF_K_{%s} = PK_{%s}
    series PF_K_{%s}_{%scenario} = PK_{%s}_{%scenario}
    For %var WAGES RSC I F_K
        series {%var}_VAL_{%s} = P{%var}_{%s} * {%var}_{%s}
        series {%var}_VAL_{%s}_{%scenario} = P{%var}_{%s}_{%scenario} * {%var}_{%s}_{%scenario}
    next 
    series F_L_VAL_{%s} = WAGES_VAL_{%s}
    series F_L_VAL_{%s}_{%scenario} = WAGES_VAL_{%s}_{%scenario}

    !step = 0  
    For %var VA NTAXS WAGES RSC F_L I Rdep F_K GOS NOS

    if %var = "Rdep" then
      {%obj}{%year}S{%scenario}(firstrow + ncom + 1 + !step,!col) = @elem({%var}_{%s},%year)

      !step=!step+1
    else

      if @isobject(%var+"_"+%s+"_"+%scenario)=1 then
      ' if @elem({%var}_{%s},%baseyear) <> 0 then
          {%obj}{%year}S{%scenario}(firstrow + ncom + 1 + !step,!col) = @elem({%var}_{%s}_{%scenario},%year)
          {%obj}{%year}S{%scenario}(firstrow + ncom + 1 + !step + !jumpval,!col) = @elem({%var}_VAL_{%s}_{%scenario},%year)

      else
          {%obj}{%year}S{%scenario}(firstrow + ncom + 1 + !step,!col) = @elem({%var}_{%s},%year)
          {%obj}{%year}S{%scenario}(firstrow + ncom + 1 + !step + !jumpval,!col) = @elem({%var}_VAL_{%s},%year)
      endif
    !step=!step+1
    endif

    next

  !col=!col+1
  next

  ' Load matrix of Imports
  !row=firstrow
  For %c {%list_com}

    if @elem(M_{%c},%baseyear) <> 0 then
      ' For total
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(M_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PM_{%c}_{%scenario},%year) * @elem(M_{%c}_{%scenario},%year)

      ' For imported
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(M_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PM_{%c}_{%scenario},%year) * @elem(M_{%c}_{%scenario},%year)

    else
      ' For total
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0

      ' For imported
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif

    !row=!row+1
  next

  if @elem(M,%baseyear) <> 0 then
      ' For total
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(M_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PM_{%scenario},%year) * @elem(M_{%scenario},%year)

      ' For imported
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(M_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PM_{%scenario},%year) * @elem(M_{%scenario},%year)

  else
      ' For total
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0

      ' For imported
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0

  endif

  !col=!col+1

  ' Load matrix of Margins paid
  For %m {%list_com_MARG}
    !row=firstrow
    For %c {%list_com}

      ' For total
      if @elem(MGP_{%m}_{%c},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row,!col) = @elem(MGP_{%m}_{%c}_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PMGP_{%m}_{%c}_{%scenario},%year) * @elem(MGP_{%m}_{%c}_{%scenario},%year)
      else
        {%obj}{%year}S{%scenario}(!row,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
      endif

      ' For Domestic
      if @elem(MGPD_{%m}_{%c},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(MGPD_{%m}_{%c}_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PMGPD_{%m}_{%c}_{%scenario},%year) * @elem(MGPD_{%m}_{%c}_{%scenario},%year)
      else
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
      endif

      ' For Imported      
      if @elem(MGPM_{%m}_{%c},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(MGPM_{%m}_{%c}_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PMGPM_{%m}_{%c}_{%scenario},%year) *  @elem(MGPM_{%m}_{%c}_{%scenario},%year)
      else
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
      endif

      !row=!row+1
    next

  !col=!col+1
  next


  ' Load matrix of Taxes less subsidies
  !row=firstrow
  For %c {%list_com}

    ' For total
    if @elem(NTAXC_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(NTAXC_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(NTAXC_VAL_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @elem(NTAXCD_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(NTAXCD_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(NTAXCD_VAL_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
    endif

    ' For Imported      
    if @elem(NTAXCM_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(NTAXCM_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(NTAXCM_VAL_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif

    !row=!row+1
  next

  ' For total
  if @elem(NTAXC,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(NTAXC_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PNTAXC_{%scenario},%year) * @elem(NTAXC_{%scenario},%year)
  else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
  endif

  ' ##### Variable not defined in ThreeME
  ' For Domestic
  'if @elem(NTAXCD,%baseyear) <> 0 then
  '    {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(NTAXCD_{%scenario},%year)
  'else
  '    {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
  'endif

  ' For Imported      
  'if @elem(NTAXCM,%baseyear) <> 0 then
  '    {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(NTAXCM_{%scenario},%year)
  'else
  '    {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
  'endif


  !col=!col+1

  ' Load matrix of Margins supplied
  !row=firstrow
  For %c {%list_com}

    ' For total
    if @elem(MGS_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(MGS_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PMGS_{%c}_{%scenario},%year) * @elem(MGS_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @elem(MGSD_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(MGSD_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PMGSD_{%c}_{%scenario},%year) * @elem(MGSD_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
    endif

    ' For Imported      
    if @elem(MGSM_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(MGSM_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PMGSM_{%c}_{%scenario},%year) * @elem(MGSM_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif
    !row=!row+1
  next

  ' For total
  if @elem(MGS,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(MGS_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PMGS_{%scenario},%year) * @elem(MGS_{%scenario},%year)
  else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
  endif

  ' For Domestic
  if @elem(MGSD,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(MGSD_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PMGSD_{%scenario},%year) * @elem(MGSD_{%scenario},%year)
  else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
  endif

  ' For Imported      
  if @elem(MGSM,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(MGSM_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PMGSM_{%scenario},%year) * @elem(MGSM_{%scenario},%year)
  else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
  endif

  !col=!col+1
  ' Load matrix of Intermediate use
  For %s {%list_sec}

    !row=firstrow 
    For %c {%list_com}

      ' For total
      if @elem(CI_{%c}_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row,!col) = @elem(CI_{%c}_{%s}_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PCI_{%c}_{%s}_{%scenario},%year) * @elem(CI_{%c}_{%s}_{%scenario},%year)
      else
        {%obj}{%year}S{%scenario}(!row,!col) = 0      
        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0      
      endif

      ' For Domestic
      if @elem(CID_{%c}_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(CID_{%c}_{%s}_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PCID_{%c}_{%s}_{%scenario},%year) * @elem(CID_{%c}_{%s}_{%scenario},%year)
      else
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0      
        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0      
      endif

      ' For Imported      
      if @elem(CIM_{%c}_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(CIM_{%c}_{%s}_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PCIM_{%c}_{%s}_{%scenario},%year) * @elem(CIM_{%c}_{%s}_{%scenario},%year)
      else
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0      
        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0      
      endif

    !row=!row+1
    next

    ' For total
    if @elem(CI_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row,!col) = @elem(CI_{%s}_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PCI_{%s}_{%scenario},%year) * @elem(CI_{%s}_{%scenario},%year)
    else
        {%obj}{%year}S{%scenario}(!row,!col) = 0      
        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0      
    endif

    ' For Domestic
    if @elem(CID_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(CID_{%s}_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PCID_{%s}_{%scenario},%year) * @elem(CID_{%s}_{%scenario},%year)
    else
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0      
        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0      
    endif

    ' For Imported      
    if @elem(CIM_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(CIM_{%scenario},%year)
        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PCIM_{%scenario},%year) * @elem(CIM_{%scenario},%year)
    else
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0      
        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0      
    endif

  !col=!col+1
  next

  ' Load matrix of Households final consumption
  !row=firstrow
  For %c {%list_com}

   ' For total
    if @elem(CH_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(CH_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PCH_{%c}_{%scenario},%year) *  @elem(CH_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @elem(CHD_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(CHD_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PCHD_{%c}_{%scenario},%year) * @elem(CHD_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
    endif

    ' For Imported      
    if @elem(CHM_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(CHM_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PCHM_{%c}_{%scenario},%year) * @elem(CHM_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif

  !row=!row+1
  next


   ' For total
    if @elem(CH,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(CH_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PCH_{%scenario},%year) * @elem(CH_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @elem(CHD,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(CHD_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PCHD_{%scenario},%year) * @elem(CHD_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
    endif

    ' For Imported      
    if @elem(CHM,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(CHM_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PCHM_{%scenario},%year) * @elem(CHM_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif

  !col=!col+1

  ' Load matrix of Governement final consumption
  !row=firstrow
  For %c {%list_com}

    ' For total
    if @elem(G_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(G_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PG_{%c}_{%scenario},%year) * @elem(G_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @elem(GD_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(GD_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PGD_{%c}_{%scenario},%year) * @elem(GD_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
    endif
    
    ' For Imported      
    if @elem(GM_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(GM_{%c}_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PGM_{%c}_{%scenario},%year) * @elem(GM_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif
    !row=!row+1
  next

  ' Agregate Gov consumption
    ' For total
    if @elem(G,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(G_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PG_{%scenario},%year) * @elem(G_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @elem(GD,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(GD_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PGD_{%scenario},%year) * @elem(GD_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
    endif
    
    ' For Imported      
    if @elem(GM,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(GM_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PGM_{%scenario},%year) * @elem(GM_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif

  !col=!col+1

  ' Load matrix of Investment
  For %s {%list_sec}

    !row=firstrow 
    For %c {%list_com}

      ' For total
      if @elem(I_{%c}_{%s},%baseyear) <> 0 then
        
        if @elem(IM_{%c}_{%s},%baseyear) <> 0 then
          series PI_{%c}_{%s}_{%scenario}*I_{%c}_{%s}_{%scenario} = PID_{%c}_{%s}_{%scenario}*ID_{%c}_{%s}_{%scenario} + PIM_{%c}_{%s}_{%scenario}*IM_{%c}_{%s}_{%scenario} 
        
        else
          series PI_{%c}_{%s}_{%scenario} = PID_{%c}_{%s}_{%scenario}
        endif

        {%obj}{%year}S{%scenario}(!row,!col) = @elem(I_{%c}_{%s}_{%scenario},%year)


        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PI_{%c}_{%s}_{%scenario},%year) * @elem(I_{%c}_{%s}_{%scenario},%year)

      else
        {%obj}{%year}S{%scenario}(!row,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

      ' For Domestic
      if @elem(ID_{%c}_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(ID_{%c}_{%s}_{%scenario},%year)

        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PID_{%c}_{%s}_{%scenario},%year) * @elem(ID_{%c}_{%s}_{%scenario},%year)

      else
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
      endif

      ' For Imported      
      if @elem(IM_{%c}_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(IM_{%c}_{%s}_{%scenario},%year)


        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PIM_{%c}_{%s}_{%scenario},%year) * @elem(IM_{%c}_{%s}_{%scenario},%year)

     else
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
      endif
    !row=!row+1
    next

    ' For agregate sectorial investment            
      ' For total
      if @elem(I_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row,!col) = @elem(I_{%s}_{%scenario},%year)


        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(I_{%s}_{%scenario},%year) * @elem(PI_{%s}_{%scenario},%year)

      else
        {%obj}{%year}S{%scenario}(!row,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
      endif

      ' For Domestic
      if @elem(ID_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(ID_{%s}_{%scenario},%year)


        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PID_{%s}_{%scenario},%year) * @elem(ID_{%s}_{%scenario},%year)

      else
        {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
      endif

      ' For Imported      
      if @elem(IM_{%s},%baseyear) <> 0 then
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(IM_{%s}_{%scenario},%year)


        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PIM_{%s}_{%scenario},%year) * @elem(IM_{%s}_{%scenario},%year)

      else
        {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
        {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
      endif

  !col=!col+1
  next

  ' Load matrix of Exports
  !row=firstrow
  For %c {%list_com}

    ' For total
    if @elem(X_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(X_{%c}_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PX_{%c}_{%scenario},%year) *  @elem(X_{%c}_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @elem(XD_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(XD_{%c}_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PXD_{%c}_{%scenario},%year) * @elem(XD_{%c}_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
    endif

    ' For Imported      
    if @elem(XM_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(XM_{%c}_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PXM_{%c}_{%scenario},%year) * @elem(XM_{%c}_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif

    !row=!row+1
  next

  ' For Agregate exports
    ' For total
    if @elem(X,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(X_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PX_{%scenario},%year) * @elem(X_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @elem(XD,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(XD_{%scenario},%year)



      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PXD_{%scenario},%year) * @elem(XD_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0
    endif

    ' For Imported      
    if @elem(XM,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(XM_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PXM_{%scenario},%year) * @elem(XM_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif

  !col=!col+1

  ' Load matrix of Change in stocks
  !row=firstrow
  For %c {%list_com}

    ' For total

    if @elem(DS_{%c},%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(DS_{%c}_{%scenario},%year)

      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PDS_{%c}_{%scenario},%year) * @elem(DS_{%c}_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0
    endif

    ' For Domestic
    if @isobject("DSD_"+%c+"_"+%scenario)=1 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(DSD_{%c}_{%scenario},%year)


      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PDSD_{%c}_{%scenario},%year) * @elem(DSD_{%c}_{%scenario},%year)        
    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(DSD_{%c},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PDSD_{%c},%year) * @elem(DSD_{%c},%year)
    endif

    ' For Imported      
    if @isobject("DSM_"+%c+"_"+%scenario)=1 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(DSM_{%c}_{%scenario},%year)

      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PDSM_{%c}_{%scenario},%year) * @elem(DSM_{%c}_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(DSM_{%c},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(DSM_{%c},%year) * @elem(PDSM_{%c},%year)
    endif

    !row=!row+1
  next

  ' For Agregate change in stocks
    ' For total
    if @elem(DS,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row,!col) = @elem(DS_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = @elem(PDS_{%scenario},%year) * @elem(DS_{%scenario},%year)
    else
      {%obj}{%year}S{%scenario}(!row,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpval,!col) = 0

    endif

    ' For Domestic
    if @elem(DSD,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = @elem(DSD_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = @elem(PDSD_{%scenario},%year) * @elem(DSD_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row + !jumpdom,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpdom + !jumpval,!col) = 0

    endif

    ' For Imported      
    if @elem(DSM,%baseyear) <> 0 then
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = @elem(DS_{%scenario},%year)
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = @elem(PDS_{%scenario},%year) * @elem(DS_{%scenario},%year)

    else
      {%obj}{%year}S{%scenario}(!row + !jumpimp,!col) = 0
      {%obj}{%year}S{%scenario}(!row + !jumpimp + !jumpval,!col) = 0
    endif

  ' ************ SHOW TEMPORARY RESULTS
  ' show {%obj}{%year}S{%scenario}


  ' ************ COMPILE TABLES INTO ONE TABLE
  tab{%year}S{%scenario}.copytable SUTALL !rowsutall !colsutall 


!colsutall = !colsutall + ncol + 1 
next                                ' End for loop scenario





!rowsutall = !rowsutall + nrow + 1
next                                 ' End for loop year



  ' ************ SHOW RESULTS
  show SUTALL


  ' ************ SAVE SUTALL AS A CSV FILE

  %date = @strnow("YYYY-MM-DD_HH-MI-SS")
  %pathfile = "..\..\results\SUT\"+%date

  SUTALL.save(t=csv, f) {%pathfile}


endsub

