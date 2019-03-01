' ============================================================================
' Series of subroutines used to run the model

' 'User defined subroutine (multi-scenarios, solver, targets, etc.)
' ============================================================================

' ============================================================================
' ============================================================================
' ============== RUN STANDARD SHOCKS =========================================
' ============================================================================

' The subroutines in this file set up the standard shocks used for comparing
' the model behaviour with other models.
'
' Many of these shocks amount to 1% of baseline GDP, hence they must be calculated
' from an ex-ante baseline run of the model. These subroutine should therefore be
' run after the baseline has been solved.


subroutine run_standard(string %scenario_list, scalar !excel)

  call standard_backup

  for %scenario {%scenario_list}
    %index = @str(@wfind(%scenario_list, %scenario))
    %scenario_name = "Scenario" + %scenario
    {%modelname}.scenario(n, a={%index}) %scenario_name
    call standard_shock(%scenario)
    call solvemodel(%solveopt)
    %grp = "Results" + %scenario
    call standard_outputs(%scenario, %grp, %index)
    if !excel=0 then
      show {%grp}
    endif
  next

  if !excel > 0 then
    ' Output to Excel
    %path = @addquotes(@linepath + "..\..\results\resultsStandard.vbs")
    shell(h) {%path}
  endif

endsub

subroutine standard_backup()

  smpl @all
  series EXPG_bckp = EXPG

  for %s {%list_sec}
    series RRSC_bckp_{%s} = RRSC_{%s}
  next

  series EXR_bckp = EXR
  series RINC_SOC_TAX_bckp=RINC_SOC_TAX

  for %c cagr cveh ccon crai croa cpri ccoa cele
    series RVATD_bckp_{%c} = RVATD_{%c}
    series RVATM_bckp_{%c} = RVATM_{%c}
  next

  for %c {%list_com}
    series WD_bckp_{%c} = WD_{%c}
  next

  series PWD_ccoa_bckp = PWD_ccoa

  for %c {%list_com}
    for %s {%list_sec}
      series R2_CI_CO2_bckp_{%c}_{%s} = R2_CI_CO2_{%c}_{%s}
    next
  next

  for %s {%list_sec}
    series R2_MAT_CO2_bckp_{%s} = R2_MAT_CO2_{%s}
  next

  for %s {%list_sec}
    series R2_Y_CO2_bckp_{%s} = R2_Y_CO2_{%s}
  next

  for %c {%list_com}
    series R2_CH_CO2_bckp_{%c} = R2_CH_CO2_{%c}
  next
endsub

subroutine standard_restore_backup()

  smpl @all
  EXPG = EXPG_bckp

  for %s {%list_sec}
    RRSC_{%s} = RRSC_bckp_{%s}
  next

  series EXR = EXR_bckp
  series RINC_SOC_TAX=RINC_SOC_TAX_bckp

  for %c cagr cveh ccon crai croa cpri ccoa cele
    series RVATD_{%c} = RVATD_bckp_{%c}
    series RVATM_{%c} = RVATM_bckp_{%c}
  next

  for %c {%list_com}
    series WD_{%c} = WD_bckp_{%c}
  next

  series PWD_ccoa = PWD_ccoa_bckp

  for %c {%list_com}
    for %s {%list_sec}
      series R2_CI_CO2_{%c}_{%s} = R2_CI_CO2_bckp_{%c}_{%s}
    next
  next

  for %s {%list_sec}
    series R2_MAT_CO2_{%s} = R2_MAT_CO2_bckp_{%s}
  next

  for %s {%list_sec}
    series R2_Y_CO2_{%s} = R2_Y_CO2_bckp_{%s}
  next

  for %c {%list_com}
    series R2_CH_CO2_{%c} = R2_CH_CO2_bckp_{%c}
  next
endsub

subroutine standard_shock(string %shock)

  call standard_restore_backup

  smpl {%baseyear}+1 @last

  ' 1% GDP point increase of public expenditure
  if @lower(%shock) = "expg1" then

    EXPG = EXPG + 0.01 * @elem(GDP, %baseyear)

  endif

  ' 1% GDP point decrease of the employer social security rate
  if @lower(%shock) = "rssc1" then

    for %s {%list_sec}
      RRSC_{%s} = RRSC_{%s} * (1 - 0.01 * @elem(GDP/RSC, %baseyear))
    next

  endif

  ' 10% decrease of the exchange rate
  if @lower(%shock) = "exr10" then

    EXR = EXR * 1.05

  endif

  ' Decrease of income tax by 1% of ex ante GDP
  if @lower(%shock) = "inct1" then

    RINC_SOC_TAX = RINC_SOC_TAX - @elem(0.01 * GDP/DISPINC_BT_VAL, %baseyear)

  endif

  ' Increase of VAT by 1% of ex ante GDP
  if @lower(%shock) = "vat1" then

  series RVAT_shock = (PVAT * VAT) / (PCH * CH - PVAT * VAT)
  series RVAT_shock_new = RVAT_shock  + (0.01 * @elem(GDP, %baseyear) * PGDP)/(PCH * CH - PVAT * VAT)

  for %c cagr cveh ccon crai croa cpri ccoa cele
    RVATD_{%c} = RVATD_{%c} * RVAT_shock_new / RVAT_shock
    RVATM_{%c} = RVATM_{%c} * RVAT_shock_new / RVAT_shock
  next

  endif

  ' 1% permanent increase of the world demand
  if @lower(%shock) = "wd1" then

    for %c {%list_com}
      WD_{%c} = WD_{%c} * 1.01
    next

  endif


  ' 10% increase of fossil fuel prices
  if @lower(%shock) = "ff10" then

  PWD_ccoa = PWD_ccoa * 1.1


  endif

  ' 1 GDP point increase of carbon tax
  if @lower(%shock) = "ct1" then

  for %c {%list_com}
    for %s {%list_sec}
      R2_CI_CO2_{%c}_{%s} = R2_CI_CO2_{%c}_{%s} + 0.01 * @elem(GDP, %baseyear) / (EMS_CI_CO2+EMS_MAT_CO2+EMS_Y_CO2+EMS_CH_CO2)
    next
  next

  for %s {%list_sec}
    R2_MAT_CO2_{%s} = R2_MAT_CO2_{%s} + 0.01 * @elem(GDP, %baseyear) / (EMS_CI_CO2+EMS_MAT_CO2+EMS_Y_CO2+EMS_CH_CO2)
  next

  for %s {%list_sec}
    R2_Y_CO2_{%s} = R2_Y_CO2_{%s} + 0.01 * @elem(GDP, %baseyear) / (EMS_CI_CO2+EMS_MAT_CO2+EMS_Y_CO2+EMS_CH_CO2)
  next


  for %c {%list_com}
    R2_CH_CO2_{%c} = R2_CH_CO2_{%c} + 0.01 * @elem(GDP, %baseyear) / (EMS_CI_CO2+EMS_MAT_CO2+EMS_Y_CO2+EMS_CH_CO2)
  next

  endif




  smpl @all

endsub

' ============================================================================
' +++++++++++++++++++++++++++
' Subroutine "run_baseshock"
' +++++++++++++++++++++++++++

' This subroutine performs a simple shock:
' - run the baseline scenario
' - run the shock scenario

' Possibility to use list of hypothesis (specified in the file configuration.prg)
' - Data calibration
' - Data shock

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
' +++++++++++++++++++++++++++
' Subroutine "run_scenario"
' +++++++++++++++++++++++++++

' This subroutine runs an individual scenario, baseline or shock
' Pass in "baseline" as the %scenario_name for the baseline scenario

subroutine run_scenario(string %scenario_name)

  if %scenario_name = "baseline" then

    ' Load a realistic reference scenario if requested (in configuration.prg)
    if %ref="realist" then
      call load_realist
    endif

    ' Solve the model
    call solvemodel(%solveopt)

  else

    ' Create a new scenario that can be compared with the baseline
    {%modelname}.scenario(n, a=2) {%scenario_name}

    ' Load data for the shock to be simulated
    call load_data_shocks(%scenario_name)

    call solvemodel(%solveopt)

  endif

endsub


' ============================================================================
' ============================================================================
' ================= STANDARD OUTPUTS =========================================
' ============================================================================


subroutine standard_outputs(string %scenario, string %grp_name, string %index)

  smpl {%baseyear}+1 {%baseyear}+1 {%baseyear}+2 {%baseyear}+2 {%baseyear}+3 {%baseyear}+3 {%baseyear}+5 {%baseyear}+5 {%baseyear}+10 {%baseyear}+10 {%baseyear}+35 {%baseyear}+35
  freeze(mode = overwrite, tab_results) {%grp_name}
  tab_results.save(t=txt) .\..\..\results\standard\{%scenario}.txt

  smpl @all
  group {%grp_name}_all 100*(GDP_{%index}/GDP_0-1) 100*(CH_{%index}/CH_0-1) 100*(I_{%index}/I_0-1) 100*(X_{%index}/X_0-1) 100*(M_{%index}/M_0-1) 100*((DISPINC_AT_VAL_{%index}/PCH_{%index})/(DISPINC_AT_VAL_0/PCH_0)-1) 100*(RSAV_H_VAL_{%index}-RSAV_H_VAL_0) 100*(PCH_{%index}/PCH_0-1) 100*(PY_{%index}/PY_0-1)  100*(PVA_{%index}/PVA_0-1) 100*(PCI_{%index}/PCI_0-1) 100*(PX_{%index}/PCI_0-1) 100*(PM_{%index}/PCI_0-1) 100*(W_{%index}/W_0-1) 100*((C_L_{%index}/PVA_{%index})/(C_L_0/PVA_0)-1) (F_L_{%index}-F_L_0) 100*(UnR_{%index}-UnR_0) 100*(RBal_Trade_VAL_{%index}-RBal_Trade_VAL_0) 100*(RBal_G_Prim_VAL_{%index}-RBal_G_Prim_VAL_0)
  freeze(mode = overwrite, tab_results) {%grp_name}_all
  tab_results.save(t=txt) .\..\..\results\standard\{%scenario}_all.txt

  ' smpl @all
  ' group all_results *
  ' freeze(mode = overwrite, tab_all) all_results
  ' tab_all.save(t=csv) .\..\..\results\all.csv

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
