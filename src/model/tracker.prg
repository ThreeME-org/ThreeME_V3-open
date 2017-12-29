' Subroutine track_objectives
' ---------------------------
'
'   This subroutine will adjust some given exogenous variables
' so that a single endogenous variable follows a predefined trajectory.
'
'   The subroutine assumes that the exogenous variables given are additive,
' and can therefore be negative or zero. In other words, these exogenous variables
' should behave as add factors on other exogenous variables, and should not appear
' directly in a log in an equation.
'
'   E.g. if we have d(log(Y_endo)) = d(log(X_exo)), and we adjust X_exo,
' we should rewrite the equation as d(log(Y_endo)) = d(log(X_exo + ADD_X_exo)),
' and adjust ADD_X_exo in track_objectives
'
' Important
' ---------
'
' The tracker must be initialised with a `call init_tracker` before it is used
'
' Syntax
' ------
'
' track_objectives(series objectives, string %obj_var, string %delta_var_names, string %delta_var_shares)
'
' - objectives: series containing the desired trajectory for the chosen endogenous variables.
'    It is defined as a series of growth rates (e.g. 0.003, 0.0012, etc.)
'
' - %obj_var: name of the endogenous variable that has to follow the predefined trajectory
'
' - %delta_var_names: names of the exogenous variables that can be adjusted to reproduce the predefined category.
'    They must be specified in a single string, separated by spaces (e.g. "ADD_X ADD_CH ADD_INV")
'
' - %delta_var_shares: proportions that each of the exogenous variables will bear in the adjustment.
'    They must be specified in a single string, separated by spaces (e.g. "0.4 0.25 0.35")
'
'
' Full example
' ------------
'
' Suppose we want to have GDP follow a predefined trajectory by adjusting world demand and government expenditures.
'
' First, we must integrate add factors in the world demand and government expenditures equations:
'
' # Exports
' d(log(X[c])) = d(log(WD[c] + ADD_EXPORTS * X[c] / X)) + d(SUBST_X[c]) if X[c] <> 0
'
' # Government expenditures
' d(log(EXPG)) = ADJUST(69,1)*(d(log(EXPG_trend + ADD_EXPG_trend)) - STEADYSTATE(62,1)*(DP_G_VAL - DP_G_VAL_n)*PGDP{-1}*GDP{-1} / (PG{-1}*EXPG{-1})) + (1-ADJUST(69,1))*d(log(EXPG{-1}))
'
' Then we load the GDP trajectory, e.g. in a series names GDP_trajectory
'
' Finally, after the baseline is solved, we simply have to call track_objectives as follows:
'
' call track_objectives(GDP_trajectory, "GDP", "ADD_EXPORTS ADD_EXPG_Trend", "0.5 0.5")



subroutine init_tracker()
  ' Global variables for the deltas subroutines
  scalar g_delta_var_count
  svector g_delta_var_names
  vector g_delta_var_shares
endsub

subroutine solve_between(scalar y1, scalar y2)
  smpl {y1} {y2}
  a_3me.solve
endsub

subroutine subroutine_on_delta(scalar delta, string %prefix, string %suffix)
  for !i = 1 to g_delta_var_count
	%var = g_delta_var_names(!i)
	{%prefix}{%var}{%suffix} = delta * g_delta_var_shares(!i)
  next
endsub

subroutine init_delta(string %prefix, string %suffix)
  for !i = 1 to g_delta_var_count
	%var = g_delta_var_names(!i)
	{%prefix}{%var} = {%prefix}{%var}{%suffix}
  next
endsub

subroutine create_calib(string %prefix)
  smpl @all
  for !i = 1 to g_delta_var_count
	%var = g_delta_var_names(!i)
	series {%prefix}{%var}_calib = {%prefix}{%var}
  next
endsub

subroutine reset_delta_calib()
  smpl @all
  call subroutine_on_delta(0, "", "_calib")
endsub

subroutine apply_delta(scalar delta, scalar year)
  smpl {year} {year}
  call subroutine_on_delta(delta, "", "")
  smpl @all
endsub

subroutine save_delta_to_calib(scalar delta, scalar year)
  smpl {year} {year}
  call subroutine_on_delta(delta, "", "_calib")
  smpl @all
endsub

subroutine solve_with_delta(scalar delta, scalar year)
  call apply_delta(delta, year)
  call solve_between({%baseyear}, year)
endsub

subroutine track_objectives(series objectives, string %obj_var, string %delta_var_names, string %delta_var_shares)

  ' Intitalise global variables for the deltas subroutines
  g_delta_var_names = @wsplit(%delta_var_names)
  g_delta_var_count = @rows(g_delta_var_names)
  svector tmp_shares = @wsplit(%delta_var_shares)
  delete(noerr) g_delta_var_shares
  vector(g_delta_var_count) g_delta_var_shares
  for !i = 1 to g_delta_var_count
	g_delta_var_shares(!i) = @val(tmp_shares(!i))
  next

  ' Initial blank run of the model
  call create_calib("")
  call reset_delta_calib

  for !obj_year = {%baseyear} + 1 to {%lastdate}

	call init_delta("", "_calib")
	call solve_between({%baseyear}, {%lastdate})

	' Calibrate the inital delta to be applied
	%obj_year = @str(!obj_year)
	scalar obj_growth = @elem(objectives, %obj_year)
	scalar obj_gdp = @elem({%obj_var}_0, @str(!obj_year - 1)) * (1 + obj_growth)
	scalar delta =  obj_gdp  - @elem({%obj_var}_0, %obj_year)
	scalar initial_delta = @abs(delta)

	' The bounds of the interval in which we will search for the right delta
	scalar lbound = 0
	scalar ubound = 0

	' Run the model with the initial delta
	call solve_with_delta(delta, !obj_year)

	' If we are below objective {%obj_var}, then the current delta can be used as a lower bound for the dichotomic search
	if @elem({%obj_var}_0, %obj_year) < obj_gdp then
	  lbound = delta
	  while @elem({%obj_var}_0, %obj_year) < obj_gdp
		delta = delta + initial_delta
		call apply_delta(delta, !obj_year)
		call solve_between({%baseyear}, !obj_year)
	  wend
	  ubound = delta
	  ' If we are above the objective, then the current delta is an upper bound
	else
	  ubound = delta
	  while @elem({%obj_var}_0, %obj_year) > obj_gdp
		delta = delta - initial_delta
		call apply_delta(delta, !obj_year)
		call solve_between({%baseyear}, !obj_year)
	  wend
	  lbound = delta
	endif

	' Dichotomic search for the correct delta
	while @abs(@elem({%obj_var}_0, %obj_year)  - obj_gdp) > 100
	  delta = (lbound + ubound) / 2
	  call solve_with_delta(delta, !obj_year)
	  if @elem({%obj_var}_0, %obj_year)  < obj_gdp then
		lbound = delta
	  else
		ubound = delta
	  endif
	wend

	' Save the correct delta in the calibration vectors
	call save_delta_to_calib(delta, !obj_year)

  next

endsub