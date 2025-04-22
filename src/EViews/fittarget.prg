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
  	' {%var_cont} = @elem({%var_cont}, %firstyear)	  	 
  	{%var_cont} = {%var_cont} + 0.001	

  else
  '%statusline = "Iteration "+ @str(iteration)+": Criterium = "+ @str(crit)+"; dcrit/dcont =   " + @str(dcrit_dcont)+ "; Control = "+ @str(cont)
  'statusline %statusline
  'logfittarget.append %statusline

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
' ******************************************* '
' The objective is the agregate energy consumption of sectors
if %objective = "CI_TOE_non_nrj_sect" then
logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective
'	for %s sagr sfoo stex sveh sgla sche sogo scon srai sroa sair spri spub setd
	for %s sagr
      smpl 2020 2020
      series CI_toe_{%s} = 0.959 * @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 0.923 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.751 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 0.575 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s smet
      smpl 2020 2020
      series CI_toe_{%s} = 0.909 * @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 1.067 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 1.103 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 1.185 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s schem
      smpl 2020 2020
      series CI_toe_{%s} = 0.977 * @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 0.965 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.983 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 0.953 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s spla
      smpl 2020 2020
      series CI_toe_{%s} = 1.031* @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 1.027 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.999 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 1.069 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s sgla
      smpl 2020 2020
      series CI_toe_{%s} = 0.931* @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 0.912 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.613 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 0.726 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s spap
      smpl 2020 2020
      series CI_toe_{%s} = 0.880* @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 0.666 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.834 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 1.195 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s sfoo
      smpl 2020 2020
      series CI_toe_{%s} = 0.990* @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 0.901 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.578 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 0.650 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s sroa
      smpl 2020 2020
      series CI_toe_{%s} = 1.664* @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 1.589 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 1.400 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 2.172 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s swat
      smpl 2020 2020
      series CI_toe_{%s} = 1.702* @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 1.489 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.793 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 0.397 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s sair
      smpl 2020 2020
      series CI_toe_{%s} = 1.211* @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 1.147 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.239 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 0.119 * @elem(CI_toe_{%s}, 2015) 
	next 
	for %s sres
      smpl 2020 2020
      series CI_toe_{%s} = 0.964* @elem(CI_toe_{%s}, 2015)
	  smpl 2030 2030
	  series CI_toe_{%s} = 0.895 * @elem(CI_toe_{%s}, 2015)
	  smpl 2040 2040
	  series CI_toe_{%s} = 0.739 * @elem(CI_toe_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_{%s} = 0.569 * @elem(CI_toe_{%s}, 2015) 
	next 
	scalar itersolution = 0.0000001
	while itersolution > 0
		scalar itersolution = 0
'    for %s sagr sfoo stex sveh sgla sche sogo scon srai sroa sair spri spub setd
    for %s sagr smet schem spla sgla spap sfoo sroa swat sair sres
      call fittarget("GR_PROG_base_E_"+%s, "constant", "CI_toe_"+%s, "CI_toe_"+%s+"_0","2015","2020", 0.01)
      scalar itersolution = itersolution +  iteration
	  call fittarget("GR_PROG_base_E_"+%s, "constant", "CI_toe_"+%s, "CI_toe_"+%s+"_0","2020","2030", 0.01)
      scalar itersolution = itersolution +  iteration
	  call fittarget("GR_PROG_base_E_"+%s, "constant", "CI_toe_"+%s, "CI_toe_"+%s+"_0","2030","2040", 0.01)
      scalar itersolution = itersolution +  iteration		
      call fittarget("GR_PROG_base_E_"+%s, "constant", "CI_toe_"+%s, "CI_toe_"+%s+"_0","2040","2050", 0.01)
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
'for %s sagr sfoo stex sveh sgla sche sogo scon srai sroa sair spri spub setd
for %s sagr smet schem spla sgla spap sfoo sroa swat sair sres
' for %s ind trsp ser
    string listcontrol =  listcontrol + " GR_PROG_base_E_"+%s
next

endif

' ******************************************* '
' The objective is the agregate energy consumption of sectors
if %objective = "CH_TOE_total" then
logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

      smpl 2020 2020
      series CH_toe = 0.901 * @elem(CH_toe, 2015)
	 smpl 2030 2030
	 series CH_toe = 0.627 * @elem(CH_toe, 2015)
	 smpl 2040 2040
	 series CH_toe = 0.504 * @elem(CH_toe, 2015)
      smpl 2050 2050
      series CH_toe = 0.470 * @elem(CH_toe, 2015)
 
	scalar itersolution = 0.0000001
	while itersolution > 0
		scalar itersolution = 0

      call fittarget("GR_PROG_HOUS_base", "constant", "CH_toe", "CH_toe_0","2015","2020", 0.01)
      scalar itersolution = itersolution +  iteration
	  call fittarget("GR_PROG_HOUS_base", "constant", "CH_toe", "CH_toe_0","2020","2030", 0.01)
      scalar itersolution = itersolution +  iteration
	  call fittarget("GR_PROG_HOUS_base", "constant", "CH_toe", "CH_toe_0","2030","2040", 0.01)
      scalar itersolution = itersolution +  iteration		
      call fittarget("GR_PROG_HOUS_base", "constant", "CH_toe", "CH_toe_0","2040","2050", 0.01)
      scalar itersolution = itersolution +  iteration

	%statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
	statusline %statusline
	logfittarget.append %statusline
	scalar itersolution_all = itersolution_all + itersolution
	wend
%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

    string listcontrol =  listcontrol + " GR_PROG_HOUS_base"


endif

' ******************************************* '
' The objective is the agregate energy consumption of sectors
if %objective = "CI_TOE_ce_non_nrj_sect" then
logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective
'	for %s sagr sfoo stex sveh sgla sche sogo scon srai sroa sair spri spub setd
	for %s sagr
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.893 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.897 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1.139 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 2.012 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 0.896 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1.795 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.92 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.768 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 0.974 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 2 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 0.897 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 3.337 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.764 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.025 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.435 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 0.808 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1.659 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 0.897 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 5.033 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.002 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.067 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.352 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 0.002 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 0.583 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 0.897 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 6.114 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s smet
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 0.868 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.165 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.032 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.906 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 0.906 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 1.083 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.31 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.117 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1.021 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1.021 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 1.059 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 2.052 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.901 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1.021 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 1.06 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 2.105 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.874 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1.689 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s schem
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 1.532 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 0.755 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.147 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.091 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.82 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.878 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 0.809 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.212 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.236 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.936 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.223 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 0.809 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.236 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 0.87 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.844 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.778 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.436 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 0.809 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.167 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 0.808 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 2.824 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.443 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.466 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s spla
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 1.817 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.169 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 0.907 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.956 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.94 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 1.635 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.29 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 0.971 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.942 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.546 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 1.635 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.29 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1.03 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.84 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.18 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1.219 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 1.635 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.29 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1.047 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 2.789 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.154 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1.219 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s sgla
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 0.75 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.93 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 0.999 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.912 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.404 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 0.553 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.959 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.026 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.884 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 0.553 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.959 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1.03 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.944 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.169 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 0.553 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.959 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1.047 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 2.947 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.144 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s spap
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 1.333 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.391 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 0.778 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.888 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1.397 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.41 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.141 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.417 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.128 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.41 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1.03 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 2.16 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.08 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.286 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 0 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.41 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1.047 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 3.275 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.068 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.286 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s sfoo
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 1.234 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.586 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 0.899 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.979 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1.431 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.878 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 1.301 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.233 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.796 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 2.25 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.972 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 1.398 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.533 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.121 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 2.25 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.769 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 1.512 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 2.045 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.032 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 2.25 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 0.771 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s sroa
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.719 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.381 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1.418 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 0.947 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.501 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 3.095 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1.361 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 0.918 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.812 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 0 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 8.039 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 30.797 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 0.583 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.406 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 0 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 25.599 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.018 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 0.75 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s swat
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.719 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.501 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.812 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 0 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.406 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 0 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s sair
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.719 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.501 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.812 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 0 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 1 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.406 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 0 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 1 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 1 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1 * @elem(CI_toe_chea_{%s}, 2015)
	next 
	for %s sres
      smpl 2020 2020
      series CI_toe_ccoa_{%s} = 0.932 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.683 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.011 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.909 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 2.516 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1.176 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1.63 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2030 2030
      series CI_toe_ccoa_{%s} = 0.967 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 0.728 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 1.05 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.619 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 3.04 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1.289 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 2.456 * @elem(CI_toe_chea_{%s}, 2015)
	  smpl 2040 2040
      series CI_toe_ccoa_{%s} = 0.967 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.755 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 0.631 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.178 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 3.04 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1.289 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 2.557 * @elem(CI_toe_chea_{%s}, 2015)
      smpl 2050 2050
      series CI_toe_ccoa_{%s} = 0.967 * @elem(CI_toe_ccoa_{%s}, 2015)
      series CI_toe_coil_{%s} = 1 * @elem(CI_toe_coil_{%s}, 2015)
      series CI_toe_cfue_{%s} = 1.755 * @elem(CI_toe_cfue_{%s}, 2015)
      series CI_toe_cpch_{%s} = 1 * @elem(CI_toe_cpch_{%s}, 2015)
      series CI_toe_cele_{%s} = 0.304 * @elem(CI_toe_cele_{%s}, 2015)
      series CI_toe_cgas_{%s} = 0.025 * @elem(CI_toe_cgas_{%s}, 2015)
      series CI_toe_cmga_{%s} = 1 * @elem(CI_toe_cmga_{%s}, 2015)
      series CI_toe_cbiog_{%s} = 1 * @elem(CI_toe_cbiog_{%s}, 2015)
      series CI_toe_cbiom_{%s} = 3.04 * @elem(CI_toe_cbiom_{%s}, 2015)
      series CI_toe_cbiof_{%s} = 1.289 * @elem(CI_toe_cbiof_{%s}, 2015)
      series CI_toe_chea_{%s} = 1.617 * @elem(CI_toe_chea_{%s}, 2015)
	next 	
scalar itersolution = 0.0000001
	while itersolution > 0
		scalar itersolution = 0
     for %s sagr smet schem spla sgla spap sfoo sroa swat sair sres
'for %s sres
'for %c cbiof
	for %c ccoa coil cfue cpch cele cgas cmga cbiog cbiom cbiof chea
	if @isobject("CI_toe_"+%c+"_"+%s+"_0") then
      call fittarget("GR_CI_base_"+%c+"_"+%s,"constant", "CI_toe_"+%c+"_"+%s, "CI_toe_"+%c+"_"+%s+"_0","2015","2020", 0.01)
      scalar itersolution = itersolution +  iteration
      call fittarget("GR_CI_base_"+%c+"_"+%s,"constant", "CI_toe_"+%c+"_"+%s, "CI_toe_"+%c+"_"+%s+"_0","2020","2030", 0.01)
      scalar itersolution = itersolution +  iteration
      call fittarget("GR_CI_base_"+%c+"_"+%s,"constant", "CI_toe_"+%c+"_"+%s, "CI_toe_"+%c+"_"+%s+"_0","2030","2040", 0.01)
      scalar itersolution = itersolution +  iteration
      call fittarget("GR_CI_base_"+%c+"_"+%s,"constant", "CI_toe_"+%c+"_"+%s, "CI_toe_"+%c+"_"+%s+"_0","2040","2050", 0.01)
      scalar itersolution = itersolution +  iteration
	endif
	next
	next 
	%statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
	statusline %statusline
	logfittarget.append %statusline
	scalar itersolution_all = itersolution_all + itersolution
	wend
%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline
    for %s sagr smet schem spla sgla spap sfoo sroa swat sair sres
    string listcontrol =  listcontrol + " GR_CI_base_ccoa_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_coil_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_cfue_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_cpch_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_cele_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_cgas_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_cmga_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_cbiog_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_cbiom_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_cbiof_"+%s
    string listcontrol =  listcontrol + " GR_CI_base_chea_"+%s
    next

endif


' ******************************************* '
' The objective is the agregate energy consumption of sectors
if %objective = "gas_prod_groningen" then
logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

      smpl 2016 2016
      series QD_cgas = 0.965 * @elem(QD_cgas, 2015)
      'series QD_cgas = 1 * @elem(QD_cgas, 2015)
	 smpl 2017 2017
	 series QD_cgas = 0.827 * @elem(QD_cgas, 2015)
	 'series QD_cgas = 0.9 * @elem(QD_cgas, 2015)
	 smpl 2018 2018
	 series QD_cgas = 0.708 * @elem(QD_cgas, 2015)
	 'series QD_cgas = 0.8 * @elem(QD_cgas, 2015)
      smpl 2019 2019
      series QD_cgas = 0.604 * @elem(QD_cgas, 2015)
      'series QD_cgas = 0.7 * @elem(QD_cgas, 2015)
      smpl 2020 2020
      series QD_cgas = 0.438 * @elem(QD_cgas, 2015)
      'series QD_cgas = 0.6 * @elem(QD_cgas, 2015)
      smpl 2021 2021
      series QD_cgas = 0.393 * @elem(QD_cgas, 2015)
      'series QD_cgas = 0.5 * @elem(QD_cgas, 2015)
      smpl 2022 2022
      series QD_cgas = 0.329 * @elem(QD_cgas, 2015)
      'series QD_cgas = 0.4 * @elem(QD_cgas, 2015)
' Do we want to also fix the gas supply from Groningen in 2050? I think yes, because we are planning not to use more than in 2022. 
'And we are actually scaling back to zero. 
      smpl 2023 2050
      series QD_cgas = 0.1 * @elem(QD_cgas, 2015)
 
scalar itersolution = 0.0000001
	while itersolution > 0
	 scalar itersolution = 0
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2016","2016", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2017","2017", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2018","2018", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2019","2019", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2020","2020", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2021","2021", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2022","2022", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2023","2023", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2024","2024", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2025","2025", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2026","2026", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2027","2027", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2028","2028", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2029","2029", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2030","2030", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2031","2031", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2032","2032", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2033","2033", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2034","2034", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2035","2035", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2036","2036", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2037","2037", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2038","2038", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2039","2039", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2040","2040", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2041","2041", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2042","2042", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2043","2043", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2044","2044", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2045","2045", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2046","2046", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2047","2047", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2048","2048", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2049","2049", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2050","2050", 0.1)
      scalar itersolution = itersolution +  iteration
	%statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
	statusline %statusline
	logfittarget.append %statusline
	scalar itersolution_all = itersolution_all + itersolution
	wend
%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

    string listcontrol =  listcontrol + " PCGAS"

endif




'******************************************** '

' The objective is the agregate energy consumption of sectors
if %objective = "PCGAS" then
logfittarget.append ""
logfittarget.append ### Start iterations for objective %objective

'Make sure that gas supply in Netherlands is never more than in the baseline. Use baseline numbers: 
      smpl 2016 2016
      series QD_cgas = 0.965 * @elem(QD_cgas, 2015)
	 smpl 2017 2017
	 series QD_cgas = 0.827 * @elem(QD_cgas, 2015)
	 smpl 2018 2018
	 series QD_cgas = 0.708 * @elem(QD_cgas, 2015)
      smpl 2019 2019
      series QD_cgas = 0.604 * @elem(QD_cgas, 2015)
      smpl 2020 2020
      series QD_cgas = 0.438 * @elem(QD_cgas, 2015)
      smpl 2021 2021
      series QD_cgas = 0.393 * @elem(QD_cgas, 2015)
      smpl 2022 2022
      series QD_cgas = 0.329 * @elem(QD_cgas, 2015)
' Do we want to also fix the gas supply from Groningen in 2050? I think yes, because we are planning not to use more than in 2022. 
'And we are actually scaling back to zero. 
      smpl 2023 2050
      series QD_cgas = 0.1 * @elem(QD_cgas, 2015)


scalar itersolution = 0.0000001
	while itersolution > 0
		scalar itersolution = 0
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2015","2015", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2016","2016", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2017","2017", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2018","2018", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2019","2019", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2020","2020", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2021","2021", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2022","2022", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2023","2023", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2024","2024", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2025","2025", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2026","2026", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2027","2027", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2028","2028", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2029","2029", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2030","2030", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2031","2031", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2032","2032", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2033","2033", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2034","2034", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2035","2035", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2036","2036", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2037","2037", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2038","2038", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2039","2039", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2040","2040", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2041","2041", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2042","2042", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2043","2043", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2044","2044", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2045","2045", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2046","2046", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2047","2047", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2048","2048", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2049","2049", 0.1)
      scalar itersolution = itersolution +  iteration
      call fittarget("PCGAS", "constant", "QD_cgas", "QD_cgas_0","2050","2050", 0.1)
      scalar itersolution = itersolution +  iteration
	%statusline = "Total iterations for objective "+ %objective+"  "+ @str(itersolution)
	statusline %statusline
	logfittarget.append %statusline
	scalar itersolution_all = itersolution_all + itersolution
	wend
%statusline = "### Global solution found for objective "+ %objective+" !!!!"
statusline %statusline
logfittarget.append %statusline

    string listcontrol =  listcontrol + " PCGAS"

endif

endsub
' ============================================================================
' ============================================================================
' ==============    RUN FIT TARGET AS STANDALONE       =======================
' ============================================================================
' Run as stand alone

' Subroutines needed-
include .\configuration.prg
include .\R_lists
include .\load_data
include .\run_extra
include .\solve
call R_lists
%fitscenario = "baseline"         ' "baseline" or "scenario_ADAPT_halfway" 
%standalone = "yes"
if %standalone = "yes" then

' Create log file
if @isobject("logfittarget")=1 then
  delete logfittarget
endif
text logfittarget
show logfittarget
' *************************************
' Calbration baseline scenario
' *************************************

if %fitscenario = "baseline" then

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

'2. Fit energy final consumption for non energy sectors
  'call fittarget_obj("CI_TOE_non_nrj_sect")

' 5. Fit fuel, electricity and gas final households consumption
  'call fittarget_obj("CH_TOE_total")

'7. Fit CI_toe_cXXX_sXXX for non-energy sectors and energy commodities
  'call fittarget_obj("CI_TOE_ce_non_nrj_sect")

'8 Make sure that the gas supply of Groningen in shrinking between 2015 and 2022 (and remains constant after 2022)
  'call fittarget_obj("gas_prod_groningen")

'1. Fit energy final consumption for non energy sectors
   call fittarget_obj("PCGAS")

smpl {%baseyear} @last
group control_var{!j} {listcontrol}
show control_var{!j} 
freeze(tab_control_var{!j}) control_var{!j} 
show tab_control_var{!j}
next
endif

if %fitscenario = "scenario_ADAPT_halfway" then

call run_scenario("scenario_ADAPT_halfway")

' Loop to (eventually) run several round of fit (can be used to check stability
for !j=1 to 1
' Initialization of the number of global iteration
scalar itersolution_all = 0 

' Initialize list of control variables
string listcontrol = ""
  %statusline = "##### START ROUND : "+ @str(!j) +  " #########"
  statusline %statusline
  logfittarget.append %statusline

'1. Fit energy final consumption for non energy sectors
  'call fittarget_obj("PCGAS")




smpl {%baseyear} @last
group control_var{!j} {listcontrol}
show control_var{!j} 
freeze(tab_control_var{!j}) control_var{!j} 
show tab_control_var{!j}
next
endif



' Manuel show: 
' show GDP_CONT GR_PROG_BASE_E_SAGR GR_PROG_BASE_E_SFOO GR_PROG_BASE_E_STEX GR_PROG_BASE_E_SVEH GR_PROG_BASE_E_SGLA GR_PROG_BASE_E_SCHE GR_PROG_BASE_E_SOGO GR_PROG_BASE_E_SCON GR_PROG_BASE_E_SRAI GR_PROG_BASE_E_SROA GR_PROG_BASE_E_SAIR GR_PROG_BASE_E_SPRI GR_PROG_BASE_E_SPUB GR_PROG_HOUS_BASE GR_PROG_TRSP_BASE GR_CI_BASE_CFUH GR_CI_BASE_CFUT GR_CI_BASE_CGAS GR_CI_BASE_CELE CH_HOUS_BASE_CFUH CH_TRSP_BASE_CFUT CH_HOUS_BASE_CGAS CH_HOUS_BASE_CELE CH_TRSP_BASE_CELE RSUBCD_CFUT RSUBCD_CFUH RSUBCD_CGAS RSUBCD_CELE 


' *************************************
' Calbration SNBC scenario
' *************************************
%statusline = "**** TOTAL ITERATIONS FOR ALL OBJECTIVES : "+ @str(itersolution_all)
statusline %statusline
logfittarget.append %statusline
endif   'End of %standalone condition


