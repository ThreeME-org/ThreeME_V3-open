smpl @all

%scenario_start = "2016"
%scenario_end = "2030"

delete(noerr) log_txt
text log_txt
show log_txt

delete(noerr) controls_group
group controls_group PE_Signal_0_indus PE_Signal_g_i_indus PE_Signal_g_f_indus PE_Signal_0_19 PE_Signal_g_i_19 PE_Signal_g_f_19 PE_Signal_0_20 PE_Signal_g_i_20 PE_Signal_g_f_20 PE_Signal_0_h_22 PE_Signal_g_i_h_22 PE_Signal_g_f_h_22 PE_Signal_0_h_23 PE_Signal_g_i_h_23 PE_Signal_g_f_h_23 PE_Signal_0_h_24 PE_Signal_g_i_h_24 PE_Signal_g_f_h_24 PE_Signal_0_16 PE_Signal_g_i_16 PE_Signal_g_f_16

controls_group.sheet(t)

subroutine log_begin(string %custom)
	log_txt.append Begin {%custom}
	%time = @time
	log_txt.append Starting at {%time}
	log_txt.append
endsub

subroutine log_end()
	log_txt.append
	%time = @time
	log_txt.append Finishing at {%time}
	log_txt.append
endsub

subroutine report_fit(string %var, string %control_suffix, string %year_final, scalar !error)
	%control = "PE_Signal_" + %control_suffix
	!error100 = !error *100
	!control_0 = @elem(PE_Signal_0_{%control_suffix}, %scenario_start)
	!control_g_f = @elem(PE_Signal_g_f_{%control_suffix}, %year_final)
	log_txt.append {%var} is {!error100} percent from target
	log_txt.append Control {%control} is at {!control_0} in {%scenario_start} and has a final growth rate of {!control_g_f}
endsub

subroutine hit_target(string %var, string %control_suffix, string %year_final, scalar !tgt)

	log_txt.append Solving for {%var} = {!tgt} in {%year_final} using PE_Signal_{%control_suffix} as control

	%control_0 = "PE_Signal_0_" + %control_suffix
	%control_g_f = "PE_Signal_g_f_" + %control_suffix

	!running = 1

	!two_ago_g_f = -1
	!previous_g_f = -1

	setmaxerrs 1

	while !running
		!var_final = @elem({%var}, %year_final)
		!error = @abs((!var_final - !tgt) / !tgt)

		call report_fit(%var, %control_suffix, !error)

		' Fit is good enough when < 0.5% error
		if !error < 0.005 then
			exitloop
		endif

		' Adaptive step size
		!step  = (!error > 0.05) * 0.01 + _
                         (!error <= 0.05) * (!error > 0.02) * 0.006 + _
                         (!error <= 0.02) * (!error > 0.0075) * 0.003 + _
                         (!error <= 0.0075) * 0.001

		if !var_final > !tgt then
			' Should not go above 0.17 final growth rate
			if {%control_g_f} + !step >= 0.17 then
				{%control_g_f} = 0.14
				{%control_0} = {%control_0} + 0.01
			else
				{%control_g_f} = {%control_g_f} + !step
			endif
		else
			' Should not go into negative final growth rate
			if {%control_g_f} + !step < 0 then
				if {%control_0} >= 0.01 then
					{%control_g_f} = (1 + {%control_g_f}) * ( {%control_0} / ({%control_0} - 0.01)) ^ (1 / ({%year_final} - 2010))
					{%control_0} = {%control_0} - 0.01
				else
					exitloop
				endif
			else
				{%control_g_f} = {%control_g_f} - !step
			endif
		endif

		' Check that we are bouncing between two values
		!current_g_f = @elem({%control_g_f}, %year_final)
		log_txt.append Two ago {!two_ago_g_f} Previous {!previous_g_f} Current {!current_g_f}
		if !two_ago_g_f = !current_g_f then
			{%control_g_f} = (!current_g_f + !two_ago_g_f) / 2
		endif

		a_3me.solve

		if @errorcount > 0 then
			' Final growth rate was most likely too high
			{%control_g_f} = 0.14
			{%control_0} = {%control_0} + 0.01
			clearerrs
			!running = 0
		endif

		!two_ago_g_f = !previous_g_f
		!previous_g_f = @elem({%control_g_f}, %year_final)
	wend

	setmaxerrs 0

endsub

subroutine fit_initial_cost()
	call log_begin("intiial electricity cost fit")
	'%elec_sec = "2301 2302 2303 2304 2305 2306 2307 2308"
	%elec_sec = "2302"
	for %s {%elec_sec}
		!initial_cost_{%s} = @elem(CU_MWh_{%s}_0, "2006")
		!tmp = !initial_cost_{%s}
		log_txt.append Initial cost for {%s} is {!tmp}
		!gr_k_{%s} = 0.02
		!sector_{%s}_completed = 0
	next

	log_txt.append
	!sectors_completed = 0

	while !sectors_completed < 8

		!sectors_completed = 0
		log_txt.append
		smpl 2007 2015

		for %s {%elec_sec}
			!cost_{%s}_2015 = @elem(CU_MWh_{%s}_2, "2015")

			!tmp = !cost_{%s}_2015
			log_txt.append Cost in 2015 for {%s} is {!tmp}
			!tmp = !gr_k_{%s}
			log_txt.append GR_PROG_K_{%s} is {!tmp}

			if @elem(CU_MWh_{%s}_2, "2015") > !initial_cost_{%s} then
				!gr_k_{%s} = !gr_k_{%s} + 0.02
				GR_PROG_K_{%s} = !gr_k_{%s}
			else
				!sector_{%s}_completed = 1
			endif

			!sectors_completed = !sectors_completed + !sector_{%s}_completed
		next

		log_txt.append {!sectors_completed} sectors have been fit

		smpl 2006 2015
		a_3me.solve
	wend

	call log_end
endsub

subroutine fit_initial_cost_2()

	call log_begin("intiial electricity cost fit")
	'%elec_sec = "2301 2302 2304 2305 2306 2307 2308"
	%elec_sec = "2302"

	smpl 2004 2015
	for %s {%elec_sec}
		series CU_MWh_{%s}_tgt = CU_MWh_{%s}_2
		smpl 2010 2015
		if @elem(CU_MWh_{%s}_2, "2015") > @elem(CU_MWh_{%s}_2, "2010") then
			CU_MWh_{%s}_tgt = @elem(CU_MWh_{%s}_0, "2010")
		endif
		smpl 2004 2015
		show a_3me
		a_3me.control Tdec_{%s} CU_MWh_{%s} CU_MWh_{%s}_tgt
	next

	call log_end
endsub

subroutine fit_energy()
	call log_begin("energy fit")
	call hit_target("ER_TRANS_PUBLIC_2", "16", %scenario_end, 9.3)
	call hit_target("ER_INDUS_2", "indus", %scenario_end, 26.4)
	call hit_target("ER_TERTIARY_2", "19", %scenario_end, 16.2)
	call hit_target("ER_ELEC_2", "h_23", %scenario_end, 33.9)
	call hit_target("ER_GAS_2", "h_24", %scenario_end, 33.9)
	call log_end
endsub

subroutine fit_cost()
	call log_begin("cost fit")
	series tmp

	for %s 2305 2306 '2307 2308
		log_txt.append Solving for CAPEX_{%s} trajectory using Tdec_{%s} as control
		tmp = CAPEX_{%s}_tgt
		smpl 2006 2010
		tmp = CAPEX_{%s}_2
		'smpl 2011 2029
		smpl 2011 2014
		tmp = NA

		smpl @all
		tmp.ipolate(type = log) CAPEX_{%s}_final

		a_3me.control Tdec_{%s} CAPEX_{%s} CAPEX_{%s}_final
	next
	call log_end
endsub

call fit_energy

a_3me.solve

ER_PIB = GDP_2/GDP_0-1
ER_Elec_Cost = (CU_oth_2301_2 * Y_2301_2 + CU_oth_2302_2 * Y_2302_2 + CU_oth_2303_2 * Y_2303_2 + CU_oth_2304_2 * Y_2304_2 + CU_oth_2305_2 * Y_2305_2 + CU_oth_2306_2 * Y_2306_2 + CU_oth_2307_2 * Y_2307_2 + CU_oth_2308_2 * Y_2308_2) / P_2
ER_Jobs_PT = L_14_2 + L_15_2 - (L_14_0 + L_15_0)

smpl 2016 2016 2020 2020 2030 2030
show Reporting
