' ============================================================================
' ============================================================================
' ==============    FIT TARGET       =========================================
' ============================================================================

' This subroutine allows for the model to reach a target at a given year. By interpoling a control variable between an initial and last year. Its arguments are:  
' %var_cont : Exogenous control variable used to reach the target
' %inter : interpolation method used to interpolate the control variable between the initial and last value. "constant" : the control variable is constant, otherwise (log)-Catmull-Rom spline --> see subroutine interpolate_period
' %var_target : target that the endogenous variable should reach at the last year
' %var_traj : endogenous variable that should follow the target. It should be the result variable that with the relevant scenario number (e.g. with _0 for baseline) 
' %firstyear : fisrt year
' %lastyear : last year
' !convcrit : Convergence criterion

subroutine fittarget(string %var_cont, string %inter, string %var_target, string %var_traj, string %firstyear, string %lastyear, scalar !convcrit)

%statusline = "Start fittarget for control variable "+ %var_cont+", option "+ %inter + ". Target variable "+ %var_target+ " should be matched by trajectory variable "+%var_traj+" in " + %lastyear+"."
statusline %statusline
logfittarget.append %statusline



' Initializations
smpl {%baseyear} {%lastyear}
{%modelname}.solve(o=b, g=10, m=5500, c=1e-8, z=1e-8,j=a,i=p,v=t)

scalar crit = @elem({%var_target}, %lastyear) - @elem({%var_traj}, %lastyear)
scalar cont = @elem({%var_cont}, %lastyear)
scalar dcrit_dcont =  na
!smplperiods = {%lastyear} - {%firstyear}

scalar iteration = 0
while @abs(crit) > !convcrit

  ' Correction of the control variable
  smpl {%lastyear} {%lastyear}
  if iteration = 0 then  	 
  	{%var_cont} = {%var_cont} + 0.001	
  else
  	{%var_cont} = {%var_cont} - crit/dcrit_dcont
  endif

  if !smplperiods > 0 then  

    smpl {%firstyear}+1 {%lastyear}-1 
    If %inter = "constant" then
    	{%var_cont} = @elem({%var_cont}, %lastyear)
    else
    	{%var_cont} = na
    	call interpolate_period(%var_cont, %firstyear, %lastyear)
    endif
 
  endif

  smpl {%baseyear} {%lastyear}
  {%modelname}.solve(o=b, g=10, m=5500, c=1e-8, z=1e-8,j=a,i=p,v=t)

  ' Calculation of the first derivative of the criterium with respect to the control variable
  scalar dcrit   = @elem({%var_target}, %lastyear) - @elem({%var_traj}, %lastyear) - crit
  scalar dcont = @elem({%var_cont}, %lastyear) - cont
  scalar dcrit_dcont =  dcrit / dcont 

  ' Calculation of the new criterium and the new control variable
  scalar crit = @elem({%var_target}, %lastyear) - @elem({%var_traj}, %lastyear)
  scalar cont = @elem({%var_cont}, %lastyear)

  ' Inform log file and statusline
  %statusline = "Iteration "+ @str(iteration)+": Criterium = "+ @str(crit)+"; dcrit/dcont =   " + @str(dcrit_dcont)+ "; Control = "+ @str(cont)
  statusline %statusline
  logfittarget.append %statusline



scalar iteration = iteration + 1
wend

' Inform final result in log file and statusline
if iteration = 0 then
	%statusline = " !! Solution found in "+ @str(iteration)+" iteration !!! Criterium = "+ @str(crit)+"; dcrit/dcont =   " + @str(dcrit_dcont)+ "; Control = "+ @str(cont)
else
	%statusline = "  !!!! SOLUTION FOUND !!!!" 
endif

statusline %statusline
logfittarget.append %statusline

endsub

' ********************************************************************************** '
' ********************************************************************************** '
' ********************************************************************************** '

subroutine fittarget_obj(string %objective)

' ******************************************* '
' The objective is GDP

if %objective = "gdp" then

    group controls GDP_cont
    group targets GDP
    group trajectories GDP_trend

    smpl {%baseyear} @last
    {%modelname}.mcontrol controls targets trajectories
    string listcontrol =  listcontrol + " GDP_cont"	
endif

' ******************************************* '
' The objective is the agregate energy consumption of sectors

if %objective = "CI_TOE_non_nrj_sect" then

logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

	scalar itersolution = 0.0000001
	while itersolution > 0
		scalar itersolution = 0

    %list_sec = ""
    for %s sagr sfoo stex sveh sgla sche sogo scon srai sroa sair spri spub
		' for %s ind trsp ser
			smpl 2030 2030
			series CI_toe_{%s} = 1.777 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 3.040 * @elem(CI_toe_{%s}, 2015) 

			call fittarget("GR_PROG_base_E_"+%s, "constant", "CI_toe_"+%s, "CI_toe_"+%s+"_0","2015","2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("GR_PROG_base_E_"+%s, "constant", "CI_toe_"+%s, "CI_toe_"+%s+"_0","2030","2050", 0.1)
			scalar itersolution = itersolution +  iteration

		next 

	%statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
	statusline %statusline
	logfittarget.append %statusline

  scalar itersolution_all = itersolution_all + itersolution
	wend

%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

for %s sagr sfoo stex sveh sgla sche sogo scon srai sroa sair spri spub
' for %s ind trsp ser
    string listcontrol =  listcontrol + " GR_PROG_base_E_"+%s
next

endif


' ******************************************* '
' The objective is the agregate energy consumption of households

if %objective = "CH_TOE_household_L1" then

logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

  scalar itersolution = 0.0000001
  while itersolution > 0
    scalar itersolution = 0

    smpl 2030 2030
    series CH_toe_hous = 1.777 * @elem(CH_toe_hous, 2015)
    series CH_toe_trsp + CH_toe_hous  = 1.777 * (@elem(CH_toe_trsp, 2015) + @elem(CH_toe_hous, 2015))

    smpl 2050 2050
    series CH_toe_hous = 3.040 * @elem(CH_toe_hous, 2015)
    series CH_toe_trsp + CH_toe_hous  = 3.040 * (@elem(CH_toe_trsp, 2015) + @elem(CH_toe_hous, 2015))

    call fittarget("GR_PROG_HOUS_base", "constant", "CH_toe_hous", "CH_toe_hous_0","2015","2030",0.1)
    scalar itersolution = itersolution +  iteration

    call fittarget("GR_PROG_HOUS_base", "constant", "CH_toe_hous", "CH_toe_hous_0","2030","2050",0.1)
    scalar itersolution = itersolution +  iteration

    call fittarget("GR_PROG_TRSP_base", "constant", "CH_toe_trsp", "CH_toe_trsp_0","2015","2030",0.1)
    scalar itersolution = itersolution +  iteration
    
    call fittarget("GR_PROG_TRSP_base", "constant", "CH_toe_trsp", "CH_toe_trsp_0","2030","2050",0.1)
    scalar itersolution = itersolution +  iteration

  %statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
  statusline %statusline
  logfittarget.append %statusline

  scalar itersolution_all = itersolution_all + itersolution
  wend


%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

string listcontrol =  listcontrol + " GR_PROG_HOUS_base GR_PROG_TRSP_base"

endif

' ******************************************* '
' The objective is the energy ce consumption of non-energy sectors

if %objective = "CF_CI_TOE_ce" then

logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

  scalar itersolution = 0.0000001
  while itersolution > 0
    scalar itersolution = 0

      smpl 2030 2030
      series CF_CI_toe_cfuh = 1.536 * @elem(CF_CI_toe_cfuh, 2015) 
      series CF_CI_toe_cfut = 1.536 * @elem(CF_CI_toe_cfut, 2015) 
      series CF_CI_toe_cgas = 1.799 * @elem(CF_CI_toe_cgas, 2015)       
      series CF_CI_toe_cele = 2.553 * @elem(CF_CI_toe_cele, 2015) 

      smpl 2050 2050
      series CF_CI_toe_cfuh = 2.271 * @elem(CF_CI_toe_cfuh, 2015) 
      series CF_CI_toe_cfut = 2.271 * @elem(CF_CI_toe_cfut, 2015) 
      series CF_CI_toe_cgas = 3.898 * @elem(CF_CI_toe_cgas, 2015)       
      series CF_CI_toe_cele = 4.699 * @elem(CF_CI_toe_cele, 2015) 

      call fittarget("GR_CI_base_cfuh", "constant", "CF_CI_toe_cfuh", "CF_CI_toe_cfuh_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("GR_CI_base_cfuh", "constant", "CF_CI_toe_cfuh", "CF_CI_toe_cfuh_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("GR_CI_base_cfut", "constant", "CF_CI_toe_cfut", "CF_CI_toe_cfut_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("GR_CI_base_cfut", "constant", "CF_CI_toe_cfut", "CF_CI_toe_cfut_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("GR_CI_base_cgas", "constant", "CF_CI_toe_cgas", "CF_CI_toe_cgas_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("GR_CI_base_cgas", "constant", "CF_CI_toe_cgas", "CF_CI_toe_cgas_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("GR_CI_base_cele", "constant", "CF_CI_toe_cele", "CF_CI_toe_cele_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("GR_CI_base_cele", "constant", "CF_CI_toe_cele", "CF_CI_toe_cele_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

  %statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
  statusline %statusline
  logfittarget.append %statusline

  scalar itersolution_all = itersolution_all + itersolution
  wend


%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

for %c cfuh cfut cgas cele 
    string listcontrol =  listcontrol + " GR_CI_base_"+%c
next

endif

' ******************************************* '
' The objective is the energy ce consumption of households

if %objective = "CH_TOE_ce" then

logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

  scalar itersolution = 0.0000001
  while itersolution > 0
    scalar itersolution = 0
      smpl 2030 2030
      series CH_toe_cfuh = 1.536 * @elem(CH_toe_cfuh, 2015)
      series CH_toe_cfut = 1.536 * @elem(CH_toe_cfut, 2015) 
      series CH_toe_cgas = 1.799 * @elem(CH_toe_cgas, 2015)       
      series CH_HOUS_toe_cele = 2.553 * @elem(CH_HOUS_toe_cele, 2015) 
      series CH_TRSP_toe_cele = 2.553 * @elem(CH_TRSP_toe_cele, 2015) 

      smpl 2050 2050
      series CH_toe_cfuh = 2.271 * @elem(CH_toe_cfuh, 2015)
      series CH_toe_cfut = 2.271 * @elem(CH_toe_cfut, 2015) 
      series CH_toe_cgas = 3.898 * @elem(CH_toe_cgas, 2015)       
      series CH_HOUS_toe_cele = 4.699 * @elem(CH_HOUS_toe_cele, 2015) 
      series CH_TRSP_toe_cele = 4.699 * @elem(CH_TRSP_toe_cele, 2015) 


      call fittarget("CH_HOUS_base_cfuh", "interpol", "CH_toe_cfuh", "CH_toe_cfuh_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_HOUS_base_cfuh", "interpol", "CH_toe_cfuh", "CH_toe_cfuh_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_TRSP_base_cfut", "interpol", "CH_toe_cfut", "CH_toe_cfut_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_TRSP_base_cfut", "interpol", "CH_toe_cfut", "CH_toe_cfut_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_HOUS_base_cgas", "interpol", "CH_toe_cgas", "CH_toe_cgas_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_HOUS_base_cgas", "interpol", "CH_toe_cgas", "CH_toe_cgas_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_HOUS_base_cele", "interpol", "CH_HOUS_toe_cele", "CH_HOUS_toe_cele_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_HOUS_base_cele", "interpol", "CH_HOUS_toe_cele", "CH_HOUS_toe_cele_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_TRSP_base_cele", "interpol", "CH_TRSP_toe_cele", "CH_TRSP_toe_cele_0", "2015", "2030", 0.1)
      scalar itersolution = itersolution +  iteration

      call fittarget("CH_TRSP_base_cele", "interpol", "CH_TRSP_toe_cele", "CH_TRSP_toe_cele_0", "2030", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

  %statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
  statusline %statusline
  logfittarget.append %statusline

  scalar itersolution_all = itersolution_all + itersolution
  wend

%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

string listcontrol = listcontrol + " CH_HOUS_BASE_CFUH CH_TRSP_BASE_CFUT CH_HOUS_BASE_CGAS CH_HOUS_BASE_CELE CH_TRSP_BASE_CELE"

endif

' ******************************************* '
' The objective is the volume of subsidies in the BAU

if %objective = "subsidies_bau" then

logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

  scalar itersolution = 0.0000001
  while itersolution > 0
    scalar itersolution = 0

   smpl 2020 2030

  SUBC_VOL_cfut.adjust =  0 -16.94817837  -43.18899765  -67.59550856  -86.94163175  -97.68073185  -96.44294931  -94.76458853  -92.60139067  -93.4138025 -93.85979875
  SUBC_VOL_cfuh.adjust =  -489.6644214  -1273.955387  -1578.116501  -1870.733312  -2122.179187  -2299.379045  -2383.210562  -2467.393006  -2551.433409  -2668.533299  -2787.100936
  SUBC_VOL_cele.adjust =  0 0 -489.524187 -1173.756256  -1508.855606  -1600.925125  -1715.238859  -1837.715141  -1968.936818  -2109.528352  -2260.158796
  SUBC_VOL_cgas.adjust =  -8.158303695  -257.6761949  -508.5183947  -743.4712904  -905.5403587  -936.6824982  -984.4880748  -1034.73351 -1087.543328  -1143.048407  -1201.386305
 
    
    for %c cfut cfuh cgas cele
 
      call fittarget("RSUBCD_"+%c, "interpol", "SUBC_VOL_"+%c, "SUBC_VOL_"+%c+"_0","2015","2020",0.1)

      for %year 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030
        call fittarget("RSUBCD_"+%c, "interpol", "SUBC_VOL_"+%c, "SUBC_VOL_"+%c+"_0",%year,%year,0.1)
      next
   
      scalar itersolution = itersolution +  iteration
    next 

    ' Subsidy rate constant after 2030
    smpl 2031 2050
    for %c cfut cfuh cgas cele
        series RSUBCD_{%c} = @elem(RSUBCD_{%c}, 2030)
    next 
    

  %statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
  statusline %statusline
  logfittarget.append %statusline

  scalar itersolution_all = itersolution_all + itersolution
  wend

%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

for %c cfut cfuh cgas cele
    string listcontrol =  listcontrol + " RSUBCD_"+%c
next

endif


' ******************************************* '
' The objective is the level of CO2 emission from natural gas

if %objective = "CO2_naturalgas" then

logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

  smpl 2030 2030
  series EMS_co2_cgas = 2.194 * @elem(EMS_co2_cgas, 2015)
  smpl 2050 2050
  series EMS_co2_cgas = 4.049 * @elem(EMS_co2_cgas, 2015)


  scalar itersolution = 0.0000001
  while itersolution > 0
    scalar itersolution = 0

    call fittarget("PHIY_TOE_CELE_SESO", "interp", "EMS_co2_cgas", "EMS_co2_cgas_0","2015","2030",0.1)
    scalar itersolution = itersolution +  iteration

  %statusline = "Total iterations for objective "+ %objective+" 2030: "+ @str(itersolution)
  statusline %statusline
  logfittarget.append %statusline

  scalar itersolution_all = itersolution_all + itersolution
  wend

  ' Recalculate starting value if solving error in 2031
  ' smpl 2031 2050
  ' series PHIY_TOE_CELE_SESO = @elem(PHIY_TOE_CELE_SESO, 2030)

  scalar itersolution = 0.0000001
  while itersolution > 0
    scalar itersolution = 0

    call fittarget("PHIY_TOE_CELE_SESO", "interp", "EMS_co2_cgas", "EMS_co2_cgas_0","2030","2050",0.1)
    scalar itersolution = itersolution +  iteration

  %statusline = "Total iterations for objective "+ %objective+" 2050: "+ @str(itersolution)
  statusline %statusline
  logfittarget.append %statusline

  scalar itersolution_all = itersolution_all + itersolution
  wend


%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

string listcontrol =  listcontrol + " PHIY_TOE_CELE_SESO"

endif


' ******************************************* '
' The objective is calibration of the baseyear price of the toe of cgas produced by the sector. It is not equal to 1 in the calibration

if %objective = "PhiY_cgas_sext" then

PY_toe_cgas_sext = 1
series PhiY_cgas_sext2 = PhiY_cgas_sext

show PhiY_toe_cgas_sext PhiY_cgas_sext PhiY_cgas_sext2 PY_toe_cgas_sext 

scalar crit = 1

while @abs(crit) > 0.00000000001

PhiY_cgas_sext2  = ( 0  + @elem(PhiY_cgas_sext , 2015)  + @elem(PhiY_cgas_sgas , 2015) )  * ( @elem(PY_toe_cgas_sext , 2015)  * PhiY_toe_cgas_sext )  / ( 0  + ( @elem(PY_toe_cgas_sext , 2015)  * PhiY_toe_cgas_sext )  + ( @elem(PY_toe_cgas_sgas , 2015)  * PhiY_toe_cgas_sgas ) )

scalar crit = @elem(PhiY_cgas_sext2 , "2015") - @elem(PhiY_cgas_sext , "2015")

PY_toe_cgas_sext = PY_toe_cgas_sext * (1 - 0.1*crit)

wend
endif



' Save control variables to excel
'group baseline *_cont 
'%pathfile = ".\..\..\data\Tunisia\fittarget.xlsx"
'wfsave(type=excelxml, mode=update) {%pathfile} range=baseline!a1 byrow @keep baseline @smpl {%baseyear} @last

endsub

' ============================================================================
' ============================================================================
' ==============    RUN FIT TARGET AS STANDALONE       =======================
' ============================================================================
' Run as stand alone

' Subroutines needed
include .\configuration.prg
include .\R_lists
include .\load_data
include .\run_extra
include .\solve

call R_lists



%standalone = "yes"
if %standalone = "yes" then

' Create log file
if @isobject("logfittarget")=1 then
  delete logfittarget
endif
text logfittarget
show logfittarget


call run_scenario("baseline")

' Loop to (eventually) run several round of fit (can be used to check stability
for !j=1 to 2

' Initialization of the number of global iteration
scalar itersolution_all = 0 

' Initialize list of control variables
string listcontrol = ""

  %statusline = "##### START ROUND : "+ @str(!j) +  " #########"
  statusline %statusline
  logfittarget.append %statusline

' 1. Fit the target for GDP
  call fittarget_obj("gdp")

' 2. Fit energy final consumption for non energy sectors
  call fittarget_obj("CI_TOE_non_nrj_sect")

' 4. Fit energy finale consumption for households
  call fittarget_obj("CH_TOE_household_L1")

' 5. Fit fuel, electricity and gas final sector consumption (for non-energy sectors)
  call fittarget_obj("CF_CI_TOE_ce")

' 6. Fit fuel, electricity and gas final households consumption
  call fittarget_obj("CH_TOE_ce")

' 7. Fit the subsidies target for the baseline scenario (when solution 1-4 stabilized)
  call fittarget_obj("subsidies_bau")

' 8. Fit the target from natural gas CO2 emission 
  call fittarget_obj("CO2_naturalgas")

smpl {%baseyear} @last
group control_var{!j} {listcontrol}
show control_var{!j} 

%statusline = "**** TOTAL ITERATIONS FOR ALL OBJECTIVES : "+ @str(itersolution_all)
statusline %statusline
logfittarget.append %statusline

next

' Manuel show: 
' show GDP_CONT GR_PROG_BASE_E_SAGR GR_PROG_BASE_E_SFOO GR_PROG_BASE_E_STEX GR_PROG_BASE_E_SVEH GR_PROG_BASE_E_SGLA GR_PROG_BASE_E_SCHE GR_PROG_BASE_E_SOGO GR_PROG_BASE_E_SCON GR_PROG_BASE_E_SRAI GR_PROG_BASE_E_SROA GR_PROG_BASE_E_SAIR GR_PROG_BASE_E_SPRI GR_PROG_BASE_E_SPUB GR_PROG_HOUS_BASE GR_PROG_TRSP_BASE GR_CI_BASE_CFUH GR_CI_BASE_CFUT GR_CI_BASE_CGAS GR_CI_BASE_CELE CH_HOUS_BASE_CFUH CH_TRSP_BASE_CFUT CH_HOUS_BASE_CGAS CH_HOUS_BASE_CELE CH_TRSP_BASE_CELE RSUBCD_CFUT RSUBCD_CFUH RSUBCD_CGAS RSUBCD_CELE


endif


