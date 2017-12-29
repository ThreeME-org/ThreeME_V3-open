!total_steps = 0
!total_iteration = 0

' Log table parameters
!title_row = 9
!controls_col = 0

subroutine objective_spline(scalar objective, vector controls, vector first_year_controls, string %solve_smpl, vector lbound, vector ubound, vector track_objective)
  vector(!n_elems) bound_controls
  vector(!n_elems) penultimate_controls

  %tmp = @optmessage
  log_txt.append {%tmp}

  %first_year = @word(%solve_smpl, 1)
  %before_first_year = @str({%first_year} - 1)
  %end_year = @word(%solve_smpl, 2)
  %before_end_year = @str({%end_year} - 1)

  log_txt.append First and last years in objective_value: {%first_year} - {%end_year}

  ' Projected gradient
  bound_controls = _
  @emult(@elt(controls, lbound), lbound)  + _
  @emult(@egt(controls, ubound), ubound) + _
  @emult(@filledvector(!n_elems, 1) - @elt(controls, lbound) - @egt(controls, ubound), controls)

  if {%end_year} > {%first_year} then
    !smpl_length = {%end_year} - {%first_year}
    ' Calculate average growth rate over the solved sample for each control
    ' to estimate control values for the penultimate year
    penultimate_controls = @ediv(bound_controls, @epow(@ediv(bound_controls, first_year_controls), 1 / !smpl_length))

    ' Value in final year of solved sample
    controls_mat = @unvec(bound_controls, !n_years)
    smpl {%end_year} {%end_year}
    mtos(controls_mat, {%controls_grp})
    ' Value in penultimate year of solved sample
    controls_mat = @unvec(penultimate_controls, !n_years)
    smpl {%before_end_year} {%before_end_year}
    mtos(controls_mat, {%controls_grp})

    ' Cardinal spline interpolation with controls in years {first_year - 1, first_year, end_year - 1, end_year}
    {%controls_grp}.spline(y) {%before_first_year} {%first_year} {%before_end_year} {%end_year}
  else
    ' No need for spline interpolation on a single point
    ' Value in final year of solved sample
    controls_mat = @unvec(bound_controls, !n_years)
    smpl {%end_year} {%end_year}
    mtos(controls_mat, {%controls_grp})
  endif

  smpl {%solve_smpl}
  a_3me.solve

  objective = @sum(@vec(@convert(objective_0)))
  log_txt.append {objective}

  for %y_var {%y_vars}
	series tmp = ({%y_var}_0 - {%y_var}_tgt) / {%y_var}_tgt
	!mean = @round(1000 * @mean(@convert(tmp))) / 10
	!min = @round(1000 * @min(@convert(tmp))) / 10
	!max = @round(1000 * @max(@convert(tmp))) / 10
	log_txt.append {%y_var} is {!mean} pct from target ({!min}, {!max})
  next

  log_txt.append

  ' Track the objective value
  !iter = @val(@mid(@optmessage, 11, 2))
  !max_iteration = !iter
  !total_iteration = !total_steps * !step_size + !iter
  track_objective(!iter + 1) = objective

  ' Update the log table
  'call update_log_tab(%start_year, %last_year)
endsub


subroutine calibrate_spline(string %start_year, string %last_year)
  !total_steps = 0
  !total_iteration = 0

  'call update_log_highlight(%start_year, %last_year)

  %members_controls = controls_grp.@members
  log_txt.append
  log_txt.append "Calibrating using spline interpolation over controls"
  log_txt.append
  log_txt.append Calibrating between {%start_year} and {%last_year}
  log_txt.append Key point on year {%last_year}
  log_txt.append Objectives: {%y_vars}
  log_txt.append Controls: {%members_controls}
  log_txt.append

  ' For spline control, different number of elements
  !n_years = 1
  !n_controls = {%controls_grp}.@count
  !n_elems = !n_controls * !n_years

  delete(noerr) controls_mat
  matrix(!n_years, {%controls_grp}.@count) controls_mat

  ' Initial value for controls
  delete(noerr) first_year_controls
  vector(!n_controls) first_year_controls
  delete(noerr) initial_controls
  vector(!n_elems) initial_controls
  delete(noerr) initial_controls_mat
  matrix initial_controls_mat
  delete(noerr) scaling_mat
  matrix scaling_mat

  ' Control boundaries
  delete(noerr) lbound
  vector(!n_elems) lbound
  delete(noerr) ubound
  vector(!n_elems) ubound

  ' Control and objective values
  delete(noerr) controls
  vector(!n_elems) controls
  scalar objective_value

  ' Period under control
  %solve_smpl = %start_year + " " + %last_year

  ' Track the progress of the objective function
  delete(noerr) track_objective
  vector(!n_iterations + 1) track_objective

  ' Save controls value in first year
  smpl {%start_year} {%start_year}
  first_year_controls = @vec(@convert({%controls_grp})) + 1e-7 ' Ensure controls aren't 0

  %year_before_first = @str(@val(%start_year) - 1)

  ' ' Initialise controls over the control period
  ' smpl {%start_year} {%start_year}
  ' controls = @vec(@convert({%controls_grp})) + 1e-7 ' Ensure controls aren't 0
  'initial_controls_mat = @convert({%controls_grp}) + 1e-7 ' Ensure controls aren't 0

  ' ' Scale to the last known value, to avoid breaks in the controls trajectory
  ' %year_before = @str(@val(%start_year) - 1)
  ' delete(noerr) smpl_before
  ' sample smpl_before {%year_before} {%year_before}
  ' delete(noerr) smpl_start
  ' sample smpl_start {%start_year} {%start_year}

  ' scaling_mat = @emult(@convert({%controls_grp}, smpl_before), @einv(@convert({%controls_grp}, smpl_start) + 1e-7))

  ' scaling_mat = @kronecker(@filledvector(@rows(initial_controls_mat), 1), scaling_mat)
  ' initial_controls = @vec(@emult(initial_controls_mat, scaling_mat))

  ' controls = initial_controls

  smpl {%last_year} {%last_year}

  for !i = 1 to controls_grp.@count
    !delta = @elem({%growth_grp}(!i), %year_before_first)
	series tmp_control = (@elem(controls_grp(!i), %year_before_first) + 1e-7) * (1 - !delta) ^ (@year - {%year_before_first})
	matplace(lbound, @convert(tmp_control), 1 + !n_years * (!i - 1), 1)
	series tmp_control = (@elem(controls_grp(!i), %year_before_first) + 1e-7) * (1 + !delta) ^ (@year - {%year_before_first})
	matplace(ubound, @convert(tmp_control), 1 + !n_years * (!i - 1), 1)
  next i

  controls = (lbound + ubound) / 2

  ' Initialise track_objective to start the optimization loop
  track_objective = 0
  track_objective(1) = 1

  smpl {%solve_smpl}

  while ((track_objective(!n_iterations +1) - track_objective(1)) / track_objective(1)) < -0.02
	' Reinitialise track_objective with each macro-iteration
	track_objective = 0
	track_objective(1) = 1
	' Initialise the iteration tracker
	!max_iteration = 0

	optimize(min=1, hess=bfgs, trust=!trust, m=!n_iterations, noerr) objective_spline(objective_value, controls, first_year_controls, %solve_smpl, lbound, ubound, track_objective)

	' Check that all iterations could be completed
	' - otherwise, we're in an endless loop
	if !max_iteration < !n_iterations then
	  log_txt.append Endless loop - abort
	  exitloop
	endif

    !total_steps = !total_steps + 1
  wend

  log_txt.append Solve for actuals after last optimize loop

  ' Solve for actuals, so that they have the latest baseline values,
  ' This means we can solve directly from the latest calibrated period in the next run
  a_3me.scenario "actuals"
  a_3me.solve
  a_3me.scenario "baseline"

  ' Update the log table
  'call update_log_tab(%start_year, %last_year)

endsub




subroutine init_log_tab(string %_start_year, string %_last_year)
  !controls_col = @wcount(%y_vars) * 3 + 2 + 1
  !n_years = {%_last_year} - {%_start_year}
  %_base_start_year = %_start_year

  ' Log table
  delete(noerr) log_tab
  table(!title_row + 55, (!controls_col + @wcount(%controls) + 3) * 2) log_tab
  log_tab.setformat(@all) ft.2
  show log_tab

  log_tab(1, 1) = "Calibrating model using BFGS optimization and spline interpolation over controls"

  log_tab(7, 1) = "Current operation: initial solve of the model"

  ' Years
  for !y = {%_start_year} to {%_last_year}
    log_tab(!title_row + 4 + !y - {%_start_year}, 1) = @str(!y)
    log_tab(!title_row + 4 + !y - {%_start_year}, !controls_col - 1) = @str(!y)
  next

  log_tab(!title_row, 2) = "Objectives"
  log_tab.setlines(!title_row + 1, 2, !title_row + 1, @wcount(%y_vars) * 3 + 1) +d
  for !j = 1 to (@wcount(%y_vars) * 3) step 3
    log_tab(!title_row + 2, !j + 1) = @word(%y_vars, (!j + 2) / 3)
    log_tab(!title_row + 2, !j + 2) = "Target"
    log_tab(!title_row + 2, !j + 3) = "% diff"
    ' Values
    call update_series_log(%_start_year, %_last_year, {%y_baseline_grp}((!j + 2) / 3), !j + 1)
    call update_series_log(%_start_year, %_last_year, {%tgt_grp}((!j + 2) / 3), !j + 2)
    call update_diff_log(%_start_year, %_last_year, {%y_baseline_grp}((!j + 2) / 3), {%tgt_grp}((!j + 2) / 3), !j + 3)
    ' Borders
    log_tab.setlines(!title_row + 2, !j + 1, !title_row + 2, !j + 1) +l
    log_tab.setlines(!title_row + 2, !j + 3, !title_row + 2, !j + 3) +r
    log_tab.setlines(!title_row + 4, !j + 1, !title_row + 4 + !n_years, !j + 1) +l
    log_tab.setlines(!title_row + 4, !j + 3, !title_row + 4 + !n_years, !j + 3) +r
  next
  log_tab.setlines(!title_row + 3, 2, !title_row + 3, @wcount(%y_vars) * 3 + 1) +d

  log_tab(!title_row, !controls_col) = "Controls"
  log_tab.setlines(!title_row + 1, !controls_col, !title_row + 1, !controls_col + @wcount(%controls) - 1) +d
  for !j = !controls_col to (!controls_col + @wcount(%controls) - 1)
    log_tab(!title_row + 2, !j) = @word(%controls, !j - !controls_col + 1)
    ' Values
    call update_series_log(%_start_year, %_last_year, {%controls_grp}(!j - !controls_col + 1), !j)
    ' Borders
    log_tab.setlines(!title_row + 2, !j, !title_row + 2, !j) +l +r
    log_tab.setlines(!title_row + 4, !j, !title_row + 4 + !n_years, !j) +r +l
  next
  log_tab.setlines(!title_row + 3, !controls_col, !title_row + 3, !controls_col + @wcount(%controls) - 1) +d

  ' Final double lines
  log_tab.setlines(!title_row + 4 + !n_years + 1, 2, !title_row + 4 + !n_years + 1, @wcount(%y_vars) * 3 + 1) +d
  log_tab.setlines(!title_row + 4 + !n_years + 1, !controls_col, !title_row + 4 + !n_years + 1, !controls_col + @wcount(%controls) - 1) +d

  show log_tab
endsub

subroutine update_series_log(string %_start_year, string %_last_year, series s, scalar !col)
  for !y = {%_start_year} to {%_last_year}
    log_tab(!y - {%_start_year} + !title_row + 4, !col) = @elem(s, @str(!y))
  next
endsub

subroutine update_diff_log(string %_start_year, string %_last_year, series s, series s_tgt, scalar !col)
  for !y = {%_start_year} to {%_last_year}
    log_tab(!y - {%_start_year} + !title_row + 4, !col) = (@elem(s, @str(!y)) / @elem(s_tgt, @str(!y)) - 1) * 100
  next
endsub

subroutine update_log_highlight(string %_start_year, string %_last_year)
  log_tab.setfont(@all) -b
  log_tab.setfont(!title_row + 4 + {%_start_year} - {%main_start_year}, 1, !title_row + 4 + {%_last_year} - {%main_start_year}, !controls_col + @wcount(%controls)) +b

  show log_tab
endsub

subroutine update_log_tab(string %_start_year, string %_last_year)
  log_tab(3, 1) = "Current calibration period: " + %_start_year + " to " + %_last_year
  log_tab(4, 1) = "Control point on year " + %_last_year

  log_tab(6, 1) = "Iteration " + @str(!total_iteration)
  %tmp = @optmessage
  log_tab(7, 1) = "Current operation: " + %tmp

  for !j = 1 to (@wcount(%y_vars) * 3) step 3
    ' Values
    call update_series_log(%_start_year, %_last_year, {%y_baseline_grp}((!j + 2) / 3), !j + 1)
    call update_series_log(%_start_year, %_last_year, {%tgt_grp}((!j + 2) / 3), !j + 2)
    call update_diff_log(%_start_year, %_last_year, {%y_baseline_grp}((!j + 2) / 3), {%tgt_grp}((!j + 2) / 3), !j + 3)
  next

  for !j = !controls_col to (!controls_col + @wcount(%controls) - 1)
    call update_series_log(%_start_year, %_last_year, {%controls_grp}(!j - !controls_col + 1), !j)
  next

  show log_tab
endsub


' Entry point


if @wcount(%args) < 4 then
  @uiprompt("Error in model fit: not enough arguments")
else
  %y_vars_grp = @word(%args, 1)
  %controls_grp = @word(%args, 2)
  %growth_grp = @word(%args, 3)

  %main_start_year = @word(%args, 4)
  %main_end_year = @word(%args, 5)

  %step_size = @word(%args, 6)

  %y_vars = {%y_vars_grp}.@members
  %controls = {%controls_grp}.@members

  %tgt_grp =  @getnextname("target")
  group {%tgt_grp}
  for %y {%y_vars}
    {%tgt_grp}.add {%y}_tgt
  next

  %y_baseline_grp =  @getnextname("baseline")
  group {%y_baseline_grp}
  for %y {%y_vars}
    {%y_baseline_grp}.add {%y}_0
  next


  if @wfindnc(a_3me.@endoglist, "objective") = 0 then
    series objective
    %objective_eq = "objective = 0"
    for %y {%y_vars}
      %objective_eq = %objective_eq + " + (" + %y + " - " + %y + "_tgt) * " + "(" + %y + " - " + %y + "_tgt)"
    next
    a_3me.append {%objective_eq}
    smpl 2006 @last
    a_3me.solve
    ' Need to make sure that actuals actually have the baseline values
    a_3me.scenario "actuals"
	a_3me.solve
    a_3me.scenario "baseline"
  endif

  !delta = 0.3
  ' Initial trust region
  !trust = 0.1
  ' Optimization happen in batch of !n_iterations
  !n_iterations = 4
  ' Check that all iterations have been made - otherwise, we're in an endless loop
  !max_iteration = 0

  !step_size = {%step_size}

  ' Log
  delete(noerr) log_txt
  text log_txt
  show log_txt
  log_txt.append Minimization using BFGS
  log_txt.append

  'call init_log_tab(%main_start_year, %main_end_year)

  if %main_start_year = %main_end_year then
    call calibrate_spline(%main_start_year, %main_end_year)
  else
    for !y = {%main_start_year} to ({%main_end_year} - 1) step !step_size
      %start_year = @str(!y)
      %last_year = @str(!y + !step_size)
      call calibrate_spline(%start_year, %last_year)
    next
  endif
endif