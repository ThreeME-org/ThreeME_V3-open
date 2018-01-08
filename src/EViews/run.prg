' *******************************
' Main entry point to the model
' Performs a complete model run:
' - loads the data calibration specified
' - loads the specification of the model
' - makes a first run to calculate the baseline (optionnally with a realistic baseline), with outputs if requested
' - makes a second run for the %data_shock specified, with outputs if requested
'NB: all configuration is to be made in the general configuration.prg file, not here

subroutine run(string %data_calibration, string %data_shock)

  ' ***********************
  ' Create the Workfile
  %wfname = ".\..\..\results\"+%modelname+"_"+%DC
  wfcreate(wf=%wfname, page=%modelname) {%freq} {%firstdate} {%lastdate}

  call create_lists


  ' ******************
  ' Load the model

if %load="new"  then
    'Give a name to the Model
    model {%modelname}
    ' Load calibration data from the Excel file
    call load_calibration

    ' Export all variables to a csv file (used by the external compiler)
    call export_all_to_csv

    ' Create the series using the dependencies (add-ins "series")
    {%modelname}.series round0 round1 round2 demography government household exceptions_fr_data ghg carbon_tax prices exceptions_tracker

    ' Export all variables to a csv file (used by the external compiler)
    call export_all_to_csv

    ' Load the model specification from the model/ folder
    {%modelname}.load blocks

    ' Save the workfile for memory
    wfsave {%wfname}

else

    ' If the data are already initailized as a Workfile with the option  %load = ""
    ' Load the data
    wfopen {%wfname}    ' Load workfile if already created (the workfile should be in ThreeME_V3\results\)

endif

  smpl @all

  ' Clean up results folder
  %cmd = @left(@linepath, 2) + " & cd " + @addquotes(@linepath) + " & del /Q ..\..\results\*.txt"
  shell(h) {%cmd}

  '************************************************
  '*********** SOLVE SCENARIOS ********************
  '************************************************
  !scenario = 0

  ' ***************************************
  ' Call here the subroutine you want to use to solve the shock

      call run_baseshock ' Perform a baseline and a shock

          ' call run_scenario("baseline")
          ' call run_enr("PPE_ENRhaut")
 

''   call additional_outputs


  ' *******************
  ' Error reporting

  string a_errors="Number of errors: "+@str(@errorcount)

  !error_count = @errorcount
  if !error_count > 0 then
    logmode all
    logsave errors
  endif


  ' **********************
  ' Saving and cleanup

''    Wfsave(c) output_{%DC}\{%DS}.wf1

''    close @all

endsub


' ============================================================================ 
' ============================================================================ 


subroutine run_baseshock


    For %DS {%shocks}

          wfopen {%wfname}

          ' Run the baseline scenario
          call run_scenario("baseline")

          ' Run scenario
          call run_scenario(%DS)

    next


endsub


' ============================================================================ 
' ============================================================================ 


subroutine run_standard(string %scenario_list)

  call standard_backup

  for %scenario {%scenario_list}
    %index = @str(@wfind(%scenario_list, %scenario))
    %scenario_name = "Scenario" + %scenario
    {%modelname}.scenario(n, a={%index}) %scenario_name
    call standard_shock(%scenario)
    call solvemodel(%solveopt)
    %grp = "Results" + %scenario
    call standard_outputs(%grp, %index)
  next

  ' Output to Excel
  %path = @addquotes(@linepath + "..\..\results\results.vbs")
  'shell(h) {%path}

endsub


' ============================================================================ 
' ============================================================================ 

subroutine usesolver

    wfopen {%wfname}
  'run the baseline scenario
  call run_scenario ("baseline")
  
  'to calibrate the baseline scenario on the past period'
  call load_xl("YQ_tgt", "series")
  
 'for %s 03 04 06
  'a_3me.control WD_03 YQ_03 YQ_03_tgt
 'next 
  
  smpl @all
  series growth03 = 0.3
  series growth04 = 0.4
  series growth07 = 0.7
  group objectives_grp  YQ_07 YQ_08
  group controls_grp    WD_07 WD_08
  group growths_grp   growth03 growth03
  {%modelname}.fit objectives_grp controls_grp growths_grp 2006 2011 5
  
    'call calibrate_scenario("SCEN_ENR_100_2", "SCEN_ENR_100_Targets")

endsub

' ============================================================================ 
' ============================================================================ 

subroutine run_enr(string %scenario)
  wfopen {%wfname}
  call load_realist

  ' Solve for baseline
  call solvemodel(%solveopt)

  ' Start ENR scenario
  {%modelname}.scenario(n, a="2") %scenario

  ' Load PhiY, coef_losses, coef_int
  ' call load_shocks(%scenario)
  call load_xl("Calibration_PPE", "ThreeME_" + %scenario)

  ' Calibrated taxation system
  call load_xl("Calibration_PPE", "Tax_" + %scenario)

  ' Load targets for verification
  call load_xl("Calibration_PPE", "Targets_" + %scenario)

  call solvemodel(%solveopt)

  call output_template("SCEN_ENR_100_2")

  ' Output to Excel
  %path = @addquotes(@linepath + "..\..\results\results.vbs")
  'shell(h) {%path}

  series ER_PIB = GDP_2/GDP_0-1
  series ER_Elec_Cost = (CU_oth_2301_2 * Y_2301_2 + CU_oth_2302_2 * Y_2302_2 + CU_oth_2303_2 * Y_2303_2 + CU_oth_2304_2 * Y_2304_2 + CU_oth_2305_2 * Y_2305_2 + CU_oth_2306_2 * Y_2306_2 + CU_oth_2307_2 * Y_2307_2 + CU_oth_2308_2 * Y_2308_2) / P_2
  series ER_Jobs_PT = L_14_2 + L_15_2 - (L_14_0 + L_15_0)

  smpl 2006 2050 '2006 2006 2015 2015 2020 2020 2030 2030
  group Reporting_2 ER_oil_2 ER_oil_2201_2 ER_Oil_2202_2 ER_elec_2301_2 ER_elec_2302_2 ER_elec_2303_2 ER_elec_2304_2 ER_elec_2305_2 ER_elec_2306_2 ER_elec_2307_2 ER_elec_2308_2 ER_elec_2 ER_gas_2 ER_gas_2401_2 ER_gas_2402_2 ER_gas_2403_2 ER_gas_2404_2 ER_gas_2405_2 ER_gas_2406_2 ER_coal_2 ER_Total_2 _
   ER_ep_oil_2 ER_ep_oil_2201_2 ER_ep_Oil_2202_2 ER_ep_elec_2301_2 ER_ep_elec_2302_2 ER_ep_elec_2303_2 ER_ep_elec_2304_2 ER_ep_elec_2305_2 ER_ep_elec_2306_2 ER_ep_elec_2307_2 ER_ep_elec_2308_2 ER_ep_elec_2 ER_ep_gas_2 ER_ep_gas_2401_2 ER_ep_gas_2402_2 ER_ep_gas_2403_2 ER_ep_gas_2404_2 ER_ep_gas_2405_2 ER_ep_gas_2406_2 ER_ep_coal_2 ER_ep_Total_2 _
   ER_Agriculture_2 ER_Indus_2 ER_Residential_2 ER_Tertiary_2 ER_Trans_Private_2 ER_Trans_Public_2  ER_Auto_2 ER_AUTO_A_2 ER_AUTO_B_2 ER_AUTO_C_2 ER_AUTO_D_2 ER_AUTO_E_2 ER_AUTO_F_2 ER_AUTO_G_2 ER_Auto_Coal_2 ER_Auto_Oil_2 ER_Auto_Elec_2 ER_Auto_Gas_2 _
   Share_NEWAUTO_CA_2 Share_NEWAUTO_CB_2 Share_NEWAUTO_CC_2 Share_NEWAUTO_CD_2 Share_NEWAUTO_CE_2 Share_NEWAUTO_CF_2 Share_NEWAUTO_CG_2  ER_Agriculture_coal_2 ER_Indus_coal_2 ER_Residential_coal_2 ER_Tertiary_coal_2 ER_Trans_Private_coal_2 ER_Trans_Public_coal_2 ER_Agriculture_oil_2 ER_Indus_oil_2 ER_Residential_oil_2 ER_Tertiary_oil_2 ER_Trans_Private_oil_2 ER_Trans_Public_oil_2 _
   ER_Agriculture_elec_2 ER_Indus_elec_2 ER_Residential_elec_2 ER_Tertiary_elec_2 ER_Trans_Private_elec_2 ER_Trans_Public_elec_2  ER_Agriculture_gas_2 ER_Indus_gas_2 ER_Residential_gas_2 ER_Tertiary_gas_2 ER_Trans_Private_gas_2 ER_Trans_Public_gas_2 _
   ER_buil_2 ER_buil_A_2 ER_buil_B_2 ER_buil_C_2 ER_buil_D_2 ER_buil_E_2 ER_buil_F_2 ER_buil_G_2	
  Reporting_2.sheet(t)
  show Reporting_2
  
   group Reporting_0 ER_oil_0 ER_oil_2201_0 ER_Oil_2202_0 ER_elec_2301_0 ER_elec_2302_0 ER_elec_2303_0 ER_elec_2304_0 ER_elec_2305_0 ER_elec_2306_0 ER_elec_2307_0 ER_elec_2308_0 ER_elec_0 ER_gas_0 ER_gas_2401_0 ER_gas_2402_0 ER_gas_2403_0 ER_gas_2404_0 ER_gas_2405_0 ER_gas_2406_0 ER_coal_0 ER_Total_0 _
   ER_ep_oil_0 ER_ep_oil_2201_0 ER_ep_Oil_2202_0 ER_ep_elec_2301_0 ER_ep_elec_2302_0 ER_ep_elec_2303_0 ER_ep_elec_2304_0 ER_ep_elec_2305_0 ER_ep_elec_2306_0 ER_ep_elec_2307_0 ER_ep_elec_2308_0 ER_ep_elec_0 ER_ep_gas_0 ER_ep_gas_2401_0 ER_ep_gas_2402_0 ER_ep_gas_2403_0 ER_ep_gas_2404_0 ER_ep_gas_2405_0 ER_ep_gas_2406_0 ER_ep_coal_0 ER_ep_Total_0 _
   ER_Agriculture_0 ER_Indus_0 ER_Residential_0 ER_Tertiary_0 ER_Trans_Private_0 ER_Trans_Public_0  ER_Auto_0 ER_AUTO_A_0 ER_AUTO_B_0 ER_AUTO_C_0 ER_AUTO_D_0 ER_AUTO_E_0 ER_AUTO_F_0 ER_AUTO_G_0 ER_Auto_Coal_0 ER_Auto_Oil_0 ER_Auto_Elec_0 ER_Auto_Gas_0 _
   Share_NEWAUTO_CA_0 Share_NEWAUTO_CB_0 Share_NEWAUTO_CC_0 Share_NEWAUTO_CD_0 Share_NEWAUTO_CE_0 Share_NEWAUTO_CF_0 Share_NEWAUTO_CG_0  ER_Agriculture_coal_0 ER_Indus_coal_0 ER_Residential_coal_0 ER_Tertiary_coal_0 ER_Trans_Private_coal_0 ER_Trans_Public_coal_0 ER_Agriculture_oil_0 ER_Indus_oil_0 ER_Residential_oil_0 ER_Tertiary_oil_0 ER_Trans_Private_oil_0 ER_Trans_Public_oil_0 _
   ER_Agriculture_elec_0 ER_Indus_elec_0 ER_Residential_elec_0 ER_Tertiary_elec_0 ER_Trans_Private_elec_0 ER_Trans_Public_elec_0  ER_Agriculture_gas_0 ER_Indus_gas_0 ER_Residential_gas_0 ER_Tertiary_gas_0 ER_Trans_Private_gas_0 ER_Trans_Public_gas_0 _
   ER_buil_0 ER_buil_A_0 ER_buil_B_0 ER_buil_C_0 ER_buil_D_0 ER_buil_E_0 ER_buil_F_0 ER_buil_G_0
 ' Reporting_0.sheet(t)
  'show Reporting_0
   
endsub

' ============================================================================ 
' ============================================================================ 

subroutine calibrate_scenario(string %scenario, string %targets)

  wfopen {%wfname}

  ' Load a realistic reference scenario if requested
  if %ref="realist" then
    call load_realist
  endif

  ' Load PhiY, coef_losses, coef_int
  ' call load_shocks(%scenario)
  call load_xl("Calibration_100ENR", "ThreeME_100ENR")

  ' Run the baseline scenario
  'call run_scenario("baseline")
  call solvemodel(%solveopt)

  ' Load targets
  call load_xl("Calibration_100ENR", "Targets_100ENR")
  ' smpl @all
  ' read(b2,s=targets,t) .\..\..\data\shocks\{%targets}.xls 3

  smpl @all
  series growth03 = 0.3
  series growth001 = 0.01
  smpl 2006 2050

  group objective_grp q22 q23 q24
  group controls_grp tenert_22 tenert_23 tenert_24
  group growth_grp growth03 growth03 growth03 growth001 growth001 growth001

  {%modelname}.fit objective_grp controls_grp growth_grp 2006 2010 4
  '{%modelname}.fit objective_grp controls_grp growth_grp 2010 2050 5

endsub


' ************************************************
' Runs an individual scenario, baseline or shock
' Pass in "baseline" as the %scenario_name for the baseline scenario
subroutine run_scenario(string %scenario_name)

  if %scenario_name = "baseline" then
    ' Load a realistic reference scenario if requested
    if %ref="realist" then
      call load_realist
    endif

    call solvemodel(%solveopt)

   'smpl 2006 2006 2015 2015 2020 2020 2025 2025 2030 2030 2050 2050
'  group Reporting ER_oil_0 ER_oil_2201_0 ER_Oil_2202_0 ER_elec_2301_0 ER_elec_2302_0 ER_elec_2303_0 ER_elec_2304_0 ER_elec_2305_0 ER_elec_2306_0 ER_elec_2307_0 ER_elec_2308_0 ER_elec_0 ER_gas_0 ER_gas_2401_0 ER_gas_2402_0 ER_gas_2403_0 ER_gas_2404_0 ER_gas_2405_0 ER_gas_2406_0 ER_coal_0 ER_Total_0 _
'   ER_Agriculture_0 ER_Indus_0 ER_Residential_0 ER_Tertiary_0 ER_Trans_Private_0 ER_Trans_Public_0  ER_Auto_0 ER_AUTO_A_0 ER_AUTO_B_0 ER_AUTO_C_0 ER_AUTO_D_0 ER_AUTO_E_0 ER_AUTO_F_0 ER_AUTO_G_0 ER_Auto_Coal_0 ER_Auto_Oil_0 ER_Auto_Elec_0 ER_Auto_Gas_0 _
'   Share_NEWAUTO_CA_0 Share_NEWAUTO_CB_0 Share_NEWAUTO_CC_0 Share_NEWAUTO_CD_0 Share_NEWAUTO_CE_0 Share_NEWAUTO_CF_0 Share_NEWAUTO_CG_0  ER_Agriculture_coal_0 ER_Indus_coal_0 ER_Residential_coal_0 ER_Tertiary_coal_0 ER_Trans_Private_coal_0 ER_Trans_Public_coal_0 ER_Agriculture_oil_0 ER_Indus_oil_0 ER_Residential_oil_0 ER_Tertiary_oil_0 ER_Trans_Private_oil_0 ER_Trans_Public_oil_0 _
'   ER_Agriculture_elec_0 ER_Indus_elec_0 ER_Residential_elec_0 ER_Tertiary_elec_0 ER_Trans_Private_elec_0 ER_Trans_Public_elec_0  ER_Agriculture_gas_0 ER_Indus_gas_0 ER_Residential_gas_0 ER_Tertiary_gas_0 ER_Trans_Private_gas_0 ER_Trans_Public_gas_0 _
'   ER_buil_0 ER_buil_A_0 ER_buil_B_0 ER_buil_C_0 ER_buil_D_0 ER_buil_E_0 ER_buil_F_0 ER_buil_G_0
'  Reporting.sheet(t)
'  show Reporting


  else

    ' Create a new scenario that can be compared with the baseline
    {%modelname}.scenario(n, a=2) {%scenario_name}

    ' Load data for the shock to be simulated
    call load_shocks(%scenario_name)

    call solvemodel(%solveopt)

''    call output_template(%scenario_name)

  endif


endsub


' ============================================================================ 
' ============================================================================ 



subroutine load_shocks(string %scenario_name)
  ' Load data for the shock to be simulated
  call load_data_shocks(".\..\..\data\shocks\" + %scenario_name + ".xls")
endsub