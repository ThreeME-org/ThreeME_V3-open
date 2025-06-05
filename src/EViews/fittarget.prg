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
' The objective is the energy ce consumption of non-energy sectors

if %objective = "PSM" then

logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

  scalar itersolution = 0.0000001
  while itersolution > 0
    scalar itersolution = 0

      smpl 2050 2050
      series M_cnrj = 0.26 * @elem(M_cnrj, 2015) 

      call fittarget("PSM_cnrj", "interpol", "M_cnrj", "M_cnrj_0", "2015", "2050", 0.1)
      scalar itersolution = itersolution +  iteration

   
  %statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
  statusline %statusline
  logfittarget.append %statusline

  scalar itersolution_all = itersolution_all + itersolution
  wend


%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

for %c cnrj
    string listcontrol =  listcontrol + " PSM_"+%c
next

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
for !j=1 to 1

' Initialization of the number of global iteration
scalar itersolution_all = 0 

' Initialize list of control variables
string listcontrol = ""

  %statusline = "##### START ROUND : "+ @str(!j) +  " #########"
  statusline %statusline
  logfittarget.append %statusline

' 1. Fit the target for GDP
  'call fittarget_obj("gdp")

' 2. Fit energy final consumption for non energy sectors
  call fittarget_obj("PSM")

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


