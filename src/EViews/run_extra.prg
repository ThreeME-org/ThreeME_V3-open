' ============================================================================
' Series of subroutines used to run the model

' 'User defined subroutine (multi-scenarios, solver, targets, etc.)
' ============================================================================
' ============================================================================
' ============================================================================
' ==============    RUN SCENARIO     =========================================
' ============================================================================
' This subroutine runs an individual scenario, baseline or shock: define the scenario you want to simulate in a conditionality with the extra data that is loaded using load_excel()
' If the scenario is not defined in the conditinnalities, it runs "baseline-steady" by default (that is not extra file is loaded)

subroutine run_scenario(string %scenario_name)
  statusline Simulating scenario %scenario_name. 

  if %scenario_name = "baseline-steady" then
    
    smpl {%baseyear} @last
    ' Solve the model
    call solvemodel(%solveopt)
    ' Exit subroutine
    return
  endif

  if %scenario_name = "baseline" then
    ' #### Calibrate scenario baseline directly
    smpl {%baseyear}+1 @last

    ' Changes in stocks converge progressivelly to 0 
    for %c {%list_com}
      DSD_{%c} = DSD_{%c}(-1) * 0.8
      DSM_{%c} = DSM_{%c}(-1) * 0.8
    next
    ' #### Calibrate scenario baseline from an excel sheet

    ' Load the data for scenario "baseline"
    smpl {%baseyear} @last
    call load_excel("France", "scenarii", "baseline")
    ' call load_excel("France", "scenarii", "baseline_fittarget")
    ' Interpolate the variables in the list
    call interpolate("POP GDP_TREND PWD_CCOI PWD_CFUT PWD_CFUH PWD_CGAS PHIY_TOE_CELE_SEWI")

    ' #### Simulate the scenario by solving the model
    smpl {%baseyear} @last
    call solvemodel(%solveopt)
    ' Exit subroutine
    return
  endif

 if %scenario_name = "carbontax_s1" then
  ' Create a new scenario that can be compared with the baseline
   {%modelname}.scenario(n, a=2) {%scenario_name}

   call load_excel("France", "scenarii", "carbontax_s1")
   call interpolate("RCO2TAX_VOL")
 
   REDIS_CT_LS = 1
   REDIS_CT_RRSC = 1 - REDIS_CT_LS
   REDIS_CT_H = 1

   call solvemodel(%solveopt) 

   call outputs

   ' Exit subroutine
    return
 endif

 if %scenario_name = "share_elec_enr" then
  ' Create a new scenario that can be compared with the baseline
   {%modelname}.scenario(n, a=2) {%scenario_name}

    ' Load the data for scenario ENR
    call load_excel("France", "scenarii", "share_elec_enr")
    call interpolate("PHIY_TOE_CELE_SECH PHIY_TOE_CELE_SECO PHIY_TOE_CELE_SEGA PHIY_TOE_CELE_SEHY PHIY_TOE_CELE_SENU PHIY_TOE_CELE_SEOI PHIY_TOE_CELE_SEOT PHIY_TOE_CELE_SESO PHIY_TOE_CELE_SEWI")

   call solvemodel(%solveopt) 

   call outputs

   ' Exit subroutine
    return
 endif

if %scenario_name="damage" then

  ' Create a new scenario that can be compared with the baseline
   {%modelname}.scenario(n, a=2) {%scenario_name}

    ' Load the data 
    call load_excel("France", "scenarii", "sea")
    call load_excel("France", "scenarii", "flood")
    call load_excel("France", "scenarii", "claysoil")    
    call load_excel("France", "scenarii", "airtemp")
    call load_excel("France", "scenarii", "disease")
    call load_excel("France", "scenarii", "supplychain")
    call load_excel("France", "scenarii", "tourism")
    call load_excel("France", "scenarii", "yield-agri")
    call load_excel("France", "scenarii", "yield-ener")
    call load_excel("France", "scenarii", "energy")
    call load_excel("France", "scenarii", "extreme")
    call load_excel("France", "scenarii", "carbontax")
    call load_excel("France", "scenarii", "energy_mix")
    call load_excel("France", "scenarii", "world_demand")
    call load_excel("France", "scenarii", "world_prices")                
                                                                           
   call solvemodel(%solveopt) 

   call outputs
return
endif

   ' Exit subroutine

endsub

' ============================================================================
' ============================================================================
' ==============    OUTPUTS    ===============================================
' ============================================================================

subroutine outputs 

  ' Save exogenous variables for the shock  (_2) 
  %exo = {%modelname}.@exoglist
  for %series {%exo}
       series {%series}_2 = {%series}
  next


' Send results to Excel 

' HYBRID OUTPUT BLOCK
  
    %hybrid = "AUTO_2 AUTO_cfut_2 AUTO_cele_2 AUTO_cgas_2 NEWAUTO_2 NEWAUTO_cfut_2 NEWAUTO_cele_2 NEWAUTO_cgas_2 AUTO_0 AUTO_cfut_0 AUTO_cele_0 AUTO_cgas_0 NEWAUTO_0 NEWAUTO_cfut_0 NEWAUTO_cele_0 NEWAUTO_cgas_0"
    for %ecl {%list_ener_class}
      %hybrid = %hybrid + " AUTO_"+%ecl+"_cfut_2" + " AUTO_"+%ecl+"_cfut_0"
    next
    for %ecl {%list_ener_class} 
      %hybrid = %hybrid + " AUTO_"+%ecl+"_cele_2" + " AUTO_"+%ecl+"_cele_0"
    next
    for %ecl {%list_ener_class}
      %hybrid = %hybrid + " AUTO_"+%ecl+"_cgas_2" + " AUTO_"+%ecl+"_cgas_0"
    nexT
    for %ecl {%list_ener_class}
      %hybrid = %hybrid + " NEWAUTO_"+%ecl+"_cfut_2" + " NEWAUTO_"+%ecl+"_cfut_0"
    next
    for %ecl {%list_ener_class} 
      %hybrid = %hybrid + " NEWAUTO_"+%ecl+"_cele_2" + " NEWAUTO_"+%ecl+"_cele_0"
    next
    for %ecl {%list_ener_class}
      %hybrid = %hybrid + " NEWAUTO_"+%ecl+"_cgas_2" + " NEWAUTO_"+%ecl+"_cgas_0"
    next
    %hybrid = %hybrid + " BUIL_2" + " BUIL_0"
    for %ecb {%list_buil_class} 
      %hybrid = %hybrid + " BUIL_"+%ecb+"_2" + " BUIL_"+%ecb+"_0"
    next
    group Hybrid {%hybrid} 


' TRANSPORTS OUTPUT BLOCK 
%transports = " km_traveler_cair_2 km_trav_auto_LD_2+km_trav_auto_SD_2 km_traveler_SD_2 km_trav_auto_SD_2 km_traveler_croa_2 km_traveler_LD_2 km_trav_auto_LD_2 km_traveler_crai_2 km_traveler_cair_0 km_trav_auto_LD_0+km_trav_auto_SD_0 km_traveler_SD_0 km_trav_auto_SD_0 km_traveler_croa_0 km_traveler_LD_0 km_trav_auto_LD_0 km_traveler_crai_0"
group Transports {%transports}


' ENERGY MIX OUTPUT BLOCK 
' not possible to use a loop because some variables don't exist (for ex : PhiY_chea_swat_2) 
%energy_mix = " PhiY_ccoa_smin_2 PhiY_ccoa_soil_2 PhiY_ccoi_smin_2 PhiY_ccoi_soil_2 PhiY_cfut_soil_2 PhiY_cfut_sbfu_2 PhiY_cfuh_soil_2 PhiY_cgas_smet_2 PhiY_cgas_soil_2 PhiY_cgas_sgas_2 PhiY_cgas_sbga_2 PhiY_cele_senu_2 PhiY_cele_seoi_2 PhiY_cele_sega_2 PhiY_cele_seco_2 PhiY_cele_sewi_2 PhiY_cele_seso_2 PhiY_cele_sehy_2 PhiY_cele_sech_2 PhiY_cele_seot_2 PhiY_chea_sech_2 PhiY_cbio_sfor_2 PhiY_cbio_spub_2 PhiY_cote_soil_2 PhiY_cote_spub_2 PhiY_ccoa_smin_0 PhiY_ccoa_soil_0 PhiY_ccoi_smin_0 PhiY_ccoi_soil_0 PhiY_cfut_soil_0 PhiY_cfut_sbfu_0 PhiY_cfuh_soil_0 PhiY_cgas_smet_0 PhiY_cgas_soil_0 PhiY_cgas_sgas_0 PhiY_cgas_sbga_0 PhiY_cele_senu_0 PhiY_cele_seoi_0 PhiY_cele_sega_0 PhiY_cele_seco_0 PhiY_cele_sewi_0 PhiY_cele_seso_0 PhiY_cele_sehy_0 PhiY_cele_sech_0 PhiY_cele_seot_0 PhiY_chea_sech_0 PhiY_cbio_sfor_0 PhiY_cbio_spub_0 PhiY_cote_soil_0 PhiY_cote_spub_0"
group Energy_Mix {%energy_mix}


' PRIMARY ENERGY OUTPUT BLOCK 
%primary_nrj = " YG_toe_2 YG_toe_0 "    
    
    for %ce {%list_com_E}
      %primary_nrj = %primary_nrj + " YG_toe_"+%ce+"_2" + " YG_toe_"+%ce+"_0"
    next

 %primary_nrj = %primary_nrj + " YG_toe_cgas_smet_2+YG_toe_cgas_soil_2+YG_toe_cgas_sgas_2" + " YG_toe_cgas_smet_0+YG_toe_cgas_soil_0+YG_toe_cgas_sgas_0" 

' we then use the same variables as in ENERGY MIX OUTPUT     
%primary_nrj = %primary_nrj + " YG_toe_ccoa_smin_2 YG_toe_ccoa_soil_2 YG_toe_ccoi_smin_2 YG_toe_ccoi_soil_2 YG_toe_cfut_soil_2 YG_toe_cfut_sbfu_2 YG_toe_cfuh_soil_2 YG_toe_cgas_smet_2 YG_toe_cgas_soil_2 YG_toe_cgas_sgas_2 YG_toe_cgas_sbga_2 YG_toe_cele_senu_2 YG_toe_cele_seoi_2 YG_toe_cele_sega_2 YG_toe_cele_seco_2 YG_toe_cele_sewi_2 YG_toe_cele_seso_2 YG_toe_cele_sehy_2 YG_toe_cele_sech_2 YG_toe_cele_seot_2 YG_toe_chea_sech_2 YG_toe_cbio_sfor_2 YG_toe_cbio_spub_2 YG_toe_cote_soil_2 YG_toe_cote_spub_2 YG_toe_ccoa_smin_0 YG_toe_ccoa_soil_0 YG_toe_ccoi_smin_0 YG_toe_ccoi_soil_0 YG_toe_cfut_soil_0 YG_toe_cfut_sbfu_0 YG_toe_cfuh_soil_0 YG_toe_cgas_smet_0 YG_toe_cgas_soil_0 YG_toe_cgas_sgas_0 YG_toe_cgas_sbga_0 YG_toe_cele_senu_0 YG_toe_cele_seoi_0 YG_toe_cele_sega_0 YG_toe_cele_seco_0 YG_toe_cele_sewi_0 YG_toe_cele_seso_0 YG_toe_cele_sehy_0 YG_toe_cele_sech_0 YG_toe_cele_seot_0 YG_toe_chea_sech_0 YG_toe_cbio_sfor_0 YG_toe_cbio_spub_0 YG_toe_cote_soil_0 YG_toe_cote_spub_0"
group Primary_nrj {%primary_nrj} 


' FINAL ENERGY OUTPUT BLOCK 
%final_nrj = " CF_toe_2 CH_toe_2 CH_HOUS_toe_2 CH_AUTO_toe_2 CF_toe_0 CH_toe_0 CH_HOUS_toe_0 CH_AUTO_toe_0"    
    
    for %ce {%list_com_E}
      %final_nrj = %final_nrj + " CF_toe_"+%ce+"_2" + " CF_toe_"+%ce+"_0"
    next

    ' vectors mix
    for %ce {%list_com_E}
      for %s {%list_sec}
        %final_nrj = %final_nrj + " CF_toe_"+%ce+"_"+%s+"_2" + " CF_toe_"+%ce+"_"+%s+"_0"
      next
    next
  %final_nrj = %final_nrj + " CF_toe_cgas_smet_2+CF_toe_cgas_soil_2+CF_toe_cgas_sgas_2" + " CF_toe_cgas_smet_0+CF_toe_cgas_soil_0+CF_toe_cgas_sgas_0"

    ' households
     for %ce {%list_com_E}
      %final_nrj = %final_nrj + " CH_HOUS_toe_"+%ce+"_2" + " CH_AUTO_toe_"+%ce+"_2" + " CH_HOUS_toe_"+%ce+"_0" + " CH_AUTO_toe_"+%ce+"_0"
     next
     %final_nrj = %final_nrj + " CH_HOUS_toe_cghb_2" + " CH_HOUS_toe_cghb_0"

    ' aggregated firms
    for %sagg TRSP AGRF IND SER
      %final_nrj = %final_nrj + " CI_toe_"+%sagg+"_2" + " CI_toe_"+%sagg+"_0"
    next
    
    ' transport
    for %ce {%list_com_E}
      %final_nrj = %final_nrj + " CI_toe_"+%ce+"_TRSP_2" + " CI_toe_"+%ce+"_TRSP_0"
    next
    %final_nrj = %final_nrj + " CI_toe_cghbo_TRSP_2" + " CI_toe_cghbo_TRSP_0" + " CI_toe_cfuel_TRSP_2" + " CI_toe_cfuel_TRSP_0"

    ' agriculture
    for %ce ccoa ccoi cfut cfuh cgas cele chea cbio
      %final_nrj = %final_nrj + " CI_toe_"+%ce+"_AGRF_2" + " CI_toe_"+%ce+"_AGRF_0"
    next
    %final_nrj = %final_nrj + " CI_toe_cghb_AGRF_2" + " CI_toe_cghb_AGRF_0" + " CI_toe_cfuel_AGRF_2" + " CI_toe_cfuel_AGRF_0"

    ' industry
    for %ce {%list_com_E}
      %final_nrj = %final_nrj + " CI_toe_"+%ce+"_IND_2" + " CI_toe_"+%ce+"_IND_0"
    next
    %final_nrj = %final_nrj + " CI_toe_cghbo_IND_2" + " CI_toe_cghbo_IND_0" + " CI_toe_cfuel_IND_2" + " CI_toe_cfuel_IND_0"
    
    ' services
    for %ce ccoa ccoi cfut cfuh cgas cele chea cbio
     %final_nrj = %final_nrj + " CI_toe_"+%ce+"_SER_2" + " CI_toe_"+%ce+"_SER_0"
    next
    %final_nrj = %final_nrj + " CI_toe_cghb_SER_2" + " CI_toe_cghb_SER_0" + " CI_toe_cfuel_SER_2" + " CI_toe_cfuel_SER_0"

group Final_nrj {%final_nrj}


' INTENSITY EMS OUTPUT BLOCK 
' doesn't work because IEMS_CH[ghg,c] are only series 
' add Intensity_EMS in add to excel once fixed  
'for %ghg {%list_GHG} 
'  for %c {%list_com} 
'      %intensity_EMS = %intensity_EMS + " IEMS_CH_"+%ghg+"_"+%c+"_2" + " IEMS_CH_"+%ghg+"_"+%c+"_0"
'    next
'  next
'group Intensity_EMS {%intensity_EMS} 


' EMS OUTPUT BLOCK

%emissions = " EMS_2 EMS_CH_2 EMS_CH_2+EMS_CI_2 EMS_CI_2 EMS_MAT_2 EMS_Y_2 EMS_SEC_2 EMS_ETS_2 EMS_NETS_2 EMS_NETSI_2 EMS_TRSP_2-EMS_sair_2 EMS_0 EMS_CH_0 EMS_CH_0+EMS_CI_0 EMS_CI_0 EMS_MAT_0 EMS_Y_0 EMS_SEC_0 EMS_ETS_0 EMS_NETS_0 EMS_NETSI_0 EMS_TRSP_0-EMS_sair_0 " 
    for %ce ccoa cfut cfuh cgas cele cbio 
      %emissions = %emissions + " EMS_CH_"+%ce+"_2" + " EMS_CH_"+%ce+"_0"
    next
    for %ce ccoa ccoi cfut cfuh cgas cbio cote
      %emissions = %emissions + " EMS_CI_"+%ce+"_2" + " EMS_CI_"+%ce+"_0"
    next
    for %ce ccoa ccoi cfut cfuh cgas cbio cote
      for %s {%list_sec} 
        %emissions = %emissions + " EMS_CI_"+%ce+"_"+%s+"_2" + " EMS_CI_"+%ce+"_"+%s+"_0"
      next
    next
    for %ce ccoa ccoi cfut cfuh cgas cbio cote
        for %sagg AGRF IND TRSP TRSF ELE SER 
       %emissions = %emissions + " EMS_CI_"+%sagg+"_"+%ce+"_2" + " EMS_CI_"+%sagg+"_"+%ce+"_0"
      next
    next
    for %s {%list_sec} 
      %emissions = %emissions + " EMS_MAT_"+%s+"_2" + " EMS_Y_"+%s+"_2" + " EMS_MAT_"+%s+"_0" + " EMS_Y_"+%s+"_0"
    next
    for %sagg AGRF IND TRSP TRSF ELE SER
       %emissions = %emissions + " EMS_MAT_"+%sagg+"_2" + " EMS_Y_"+%sagg+"_2" + " EMS_MAT_"+%sagg+"_0" + " EMS_Y_"+%sagg+"_0"
    next
    for %s {%list_sec} 
      %emissions = %emissions + " EMS_"+%s+"_2" + " EMS_"+%s+"_0"
    next
    for %sagg AGRF IND TRSP TRSF ELE SER
      %emissions = %emissions + " EMS_"+%sagg+"_2" + " EMS_"+%sagg+"_0" 
    next
%emissions = %emissions + " EMS_CH_ccoa_2+EMS_CI_ccoa_2 EMS_CH_cfut_2+EMS_CI_cfut_2 EMS_CH_cfuh_2+EMS_CI_cfuh_2 EMS_CH_cgas_2+EMS_CI_cgas_2 EMS_CH_cbio_2+EMS_CI_cbio_2 EMS_CH_ccoa_0+EMS_CI_ccoa_0 EMS_CH_cfut_0+EMS_CI_cfut_0 EMS_CH_cfuh_0+EMS_CI_cfuh_0 EMS_CH_cgas_0+EMS_CI_cgas_0 EMS_CH_cbio_0+EMS_CI_cbio_0 "
%emissions = %emissions + " EMS_CO2_0 EMS_CI_CO2_0 EMS_Y_CO2_0 EMS_MAT_CO2_0 EMS_CH_CO2_0 EMS_CO2_2 EMS_CI_CO2_2 EMS_Y_CO2_2 EMS_MAT_CO2_2 EMS_CH_CO2_2"
  
    for %s sagr sfor sfoo sveh sgla spap sche spla smet sigo scgo scon srai sroa swat sair spri spub smin soil sbfu seoi sega seco sech seot
      %emissions = %emissions + " EMS_CI_CO2_"+%s+"_2" + " EMS_Y_CO2_"+%s+"_2" + " EMS_MAT_CO2_"+%s+"_2"
    next 

    for %s sagr sfor sfoo sveh sgla spap sche spla smet sigo scgo scon srai sroa swat sair spri spub smin soil sbfu seoi sega seco sech seot
      %emissions = %emissions + " EMS_CI_CO2_"+%s+"_0" + " EMS_Y_CO2_"+%s+"_0" + " EMS_MAT_CO2_"+%s+"_0"
    next

group Emissions {%emissions}

' MACRO OUTPUT BLOCK 
%macro = " 100*(GDP_2/GDP_0-1) 100*((VA_2-VA_spub_2)/(VA_0-VA_spub_0)-1) 100*((CH_2-CH_0)/GDP_0) 100*((CH_cveh_2-CH_cveh_0)/GDP_0) 100*((G_2-G_0)/GDP_0) 100*((I_2-I_0)/GDP_0) 100*((DS_2-DS_0)/GDP_0) 100*((X_2-X_0)/GDP_0)-100*((M_2-M_0)/GDP_0) 100*((CH_2-CH_ccon_2-(CH_0-CH_ccon_0))/GDP_0) 100*((I_2+CH_ccon_2-(I_0+CH_ccon_0))/GDP_0) 100*((I_2-IA_spub_2-(I_0-IA_spub_0))/GDP_0) 100*((CH_ccon_2-CH_ccon_0)/GDP_0) 100*((IA_spub_2-IA_spub_0)/GDP_0) 100*(CH_2/CH_0-1) 100*(CH_cveh_2/CH_cveh_0-1) 100*(G_2/G_0-1) 100*(I_2/I_0-1) 100*((I_2-IA_spub_2)/(I_0-IA_spub_0)-1) 100*(X_2/X_0-1) 100*(M_2/M_0-1) 100*((CH_2-CH_ccon_2)/(CH_0-CH_ccon_0)-1) 100*((I_2+CH_ccon_2)/(I_0+CH_ccon_0)-1) 100*((CH_ccon_2)/(CH_ccon_0)-1) 100*((IA_spub_2)/(IA_spub_0)-1) 100*(DISPINC_AT_VAL_2/DISPINC_AT_VAL_0-1) 100*(DISPINC_AT_VAL_2/DISPINC_AT_VAL_0-1)-100*(PCH_2/PCH_0-1) 100*(DISPINC_AT_VAL_2/DISPINC_AT_VAL_0-1)-100*((F_L_2/F_L_0)-1) 100*(DISPINC_AT_VAL_2/DISPINC_AT_VAL_0-1)-100*((F_L_2/F_L_0)-1)-100*(PCH_2/PCH_0-1) 100*(MPS_n_2-MPS_n_0) 100*(PCH_2/PCH_0-1) 100*(PY_2/PY_0-1) 100*(PX_2/PX_0-1) 100*(PM_2/PM_0-1) 100*(W_2/W_0-1) 100*((W_2/PCH_2)/(W_0/PCH_0)-1) 100*(C_L_2/C_L_0-1) 100*((C_L_2/PVA_2)/(C_L_0/PVA_0)-1) ((F_L_2/F_L_0)-1)*100 F_L_2-F_L_0 100*(UnR_2-UnR_0) 100*(Bal_Trade_VAL_2/(GDP_2*PGDP_2)-Bal_Trade_VAL_0/(GDP_0*PGDP_0)) 100*(ENER_BILL_2/(GDP_2*PGDP_2)-ENER_BILL_0/(GDP_0*PGDP_0)) 100*((-Bal_G_prim_VAL_2)/(GDP_2*PGDP_2)-(-Bal_G_prim_VAL_0)/(GDP_0*PGDP_0)) 100*((-Bal_G_tot_VAL_2)/(GDP_2*PGDP_2)-(-Bal_G_tot_VAL_0)/(GDP_0*PGDP_0)) (DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_0/(PGDP_0*GDP_0))*100 GDP_2 (GDPter_2/GDPbis_2-1)*100 SPEND_G_VAL_2 INC_G_VAL_2 Bal_G_tot_VAL_2 DEBT_G_VAL_2 PCH_2 PGDP_2 PY_2 PM_2 PI_2 PX_2 CU_2 F_L_2 F_K_2 F_E_2 F_MAT_2 W_2 C_L_2 C_K_2 C_E_2 C_MAT_2 (GDPter_0/GDPbis_0-1)*100 SPEND_G_VAL_0 INC_G_VAL_0 Bal_G_tot_VAL_0 DEBT_G_VAL_0 PCH_0 PGDP_0 PY_0 PM_0 PI_0 PX_0 CU_0 F_L_0 F_K_0 F_E_0 F_MAT_0 W_0 C_L_0 C_K_0 C_E_0 C_MAT_0 POP"
%macro = %macro + " Bal_G_prim_VAL_2 ENER_BILL_2 Bal_Trade_VAL_2 UnR_2 Bal_G_prim_VAL_0 ENER_BILL_0 Bal_Trade_VAL_0 UnR_0 gdp_0 "
%macro = %macro + " Y_toe_2 M_toe_2 CI_toe_2 CH_toe_2 X_toe_2 Y_toe_0 M_toe_0 CI_toe_0 CH_toe_0 X_toe_0"
group Macro {%macro}

'%index = "2"
    'group Macro 100*(GDP_{%index}/GDP_0-1) 100*(CH_{%index}/CH_0-1) 100*(I_{%index}/I_0-1) 100*(X_{%index}/X_0-1) '100*(M_{%index}/M_0-1) 100*((DISPINC_AT_VAL_{%index}/PCH_{%index})/(DISPINC_AT_VAL_0/PCH_0)-1) '100*(RSAV_H_VAL_{%index}-RSAV_H_VAL_0) 100*(PCH_{%index}/PCH_0-1) 100*(PY_{%index}/PY_0-1)  '100*(PVA_{%index}/PVA_0-1) 100*(PCI_{%index}/PCI_0-1) 100*(PX_{%index}/PX_0-1) 100*(PM_{%index}/PM_0-1) '100*(W_{%index}/W_0-1) 100*((C_L_{%index}/PVA_{%index})/(C_L_0/PVA_0)-1) (F_L_{%index}-F_L_0) '100*(UnR_{%index}-UnR_0) 100*(RBal_Trade_VAL_{%index}-RBal_Trade_VAL_0) '100*(RBal_G_Prim_VAL_{%index}-RBal_G_Prim_VAL_0) 100*(RDEBT_G_VAL_{%index}-RDEBT_G_VAL_0) '100*(EMS_CO2_{%index}/EMS_CO2_0-1) 100*(CH_0+G_0)/GDP_0*((CH_{%index}+G_{%index})/(CH_0+G_0)-1) 
    '100*I_0/GDP_0*(I_{%index}/I_0-1) 100*(X_0-M_0)/GDP_0*((X_{%index}-M_{%index})/(X_0-M_0)-1) 100*DS_0/
    'GDP_0*(DS_{%index}/DS_0-1)



' LABOR OUTPUT BLOCK 
%labor = " F_L_2 F_L_0 "    
    
    for %s {%list_sec}
      %labor = %labor + " F_L_"+%s+"_2" + " F_L_"+%s+"_0"
    next
    
    ' doesn't work (but works in EX-STRING-GROUP-2...)
    'for %sagg {%list_sec_AGGREG}
     for %sagg AGRF IND TRSP TRSF ELE SER 
        %labor = %labor + " F_L_"+%sagg+"_2" + " F_L_"+%sagg+"_0"
     next
group Labor {%labor}


' VALUE ADDED OUTPUT BLOCK 
%value_added = " VA_2 VA_0 "    
    
    for %s {%list_sec}
      %value_added = %value_added + " VA_"+%s+"_2" + " VA_"+%s+"_0"
    next
    
    ' doesn't work (but works in EX-STRING-GROUP-2...)
    'for %sagg {%list_sec_AGGREG}
    for %sagg AGRF IND TRSP TRSF ELE SER 
      %value_added = %value_added + " VA_"+%sagg+"_2" + " VA_"+%sagg+"_0"
    next
group Value_Added {%value_added}



' TOTAL INVESTMENT OUTPUT BLOCK 
%INV = " IA_2 IA_0 "    
    
    for %s {%list_sec}
      %INV = %INV + " IA_"+%s+"_2" + " IA_"+%s+"_0"
    next
    
    ' doesn't work (but works in EX-STRING-GROUP-2...)
    'for %sagg {%list_sec_AGGREG}
    for %sagg AGRF IND TRSP TRSF ELE SER 
      %INV = %INV + " IA_"+%sagg+"_2" + " IA_"+%sagg+"_0"
    next
group INV {%INV}



' CONSUMPTION OUTPUT BLOCK 
%consumption = " CH_2 CH_0 PCH_2 PCH_0 "    
    
    for %c cagr cfoo cveh cgla cpap cche cpla cmet cigo ccgo ccon crai croa cwat cair cpri cpub cmin ccoa cfut cfuh cgas cele chea cbio
      %consumption = %consumption + " CH_"+%c+"_2" + " CH_"+%c+"_0" + " PCH_"+%c+"_2" + " PCH_"+%c+"_0"
    next
group Consumption {%consumption}



'output for main baseline variables
    %baseline = "@PCH(GDP_0) @PCH(pch_0) UNR_0 RDEBT_G_VAL_0 RBal_G_Prim_VAL_0 rbal_trade_val_0 EMS_CO2_0 POP GDP_0 PCH_0 EMS_CH_CO2_0 EMS_CI_CO2_0" 

  ' Concatenation des strings
   for %c {%list_com}
   if @isobject("EMS_"+%c+"_0") = 1 then
     %baseline = %baseline + " EMS_"+%c+"_0"
   endif
   next

   for %c {%list_com}
   if @isobject("EMS_CO2_"+%c+"_0") = 1 then
     %baseline = %baseline + " EMS_CO2_"+%c+"_0"
   endif
   next



    group Baseline {%baseline}  

'CAPITAL BLOCK
%capital = " F_K_2 F_K_0"    
    
    for %s {%list_sec}
      %capital = %capital + " F_K_"+%s+"_2" + " F_K_"+%s+"_0" 
    next
     
group Capital {%capital}

'PRODUCTION BLOCK
%production = " Y_2 Y_0 "    
    
    for %s {%list_sec}
      %production = %production + " Y_"+%s+"_2" + " Y_"+%s+"_0"
    next
        
group Production {%production}
        
'output for carbon tax
%carbontax = "T2VAL_CH_0 T2VAL_CH_2 T2VAL_SEC_0 T2VAL_SEC_2 T2VAL_CI_0 T2VAL_CI_2 T2VAL_Y_0 T2VAL_Y_2 T2VAL_MAT_0 T2VAL_MAT_2"
group Carbontax {%carbontax}

'output for imports
%import = "M_2 M_0"

    for %c cagr cfor cfoo cveh cgla cpap cche cpla cmet cigo ccgo crai croa cwat cair cpri cpub cmin ccoa ccoi cfut cfuh cgas cele cbio cote
      %import = %import + " M_"+%c+"_2" + " M_"+%c+"_0"
    next

group Import {%import}

'output for exports
%export = "X_2 X_0"

    for %c cagr cfor cfoo cveh cgla cpap cche cpla cmet cigo ccgo crai croa cwat cair cpri cpub cmin ccoa ccoi cfut cfuh cgas cele cbio cote
      %export = %export + " X_"+%c+"_2" + " X_"+%c+"_0"
    next

group Export {%export}
    
call savetoexcel("Hybrid Transports Energy_Mix Primary_nrj Final_nrj Emissions Macro Labor Value_Added INV Consumption Baseline Capital Production Carbontax Import Export", "Result_France.xlsx", "YES")

endsub
' ============================================================================
' ============================================================================
' ==============    SAVE TO EXCEL    =========================================
' ============================================================================
' This subroutine copies results to excel using the template %filemane located in the directory "result". It copies the list of groups "%groupresult" (Syntax: "group1 group2 group 3") in the sheet using the name of the group. A sheet per group is created or filled in (if already existing).  If the %duplicate% = "YES", the template is replicated with a name starting with the date and hours. Otherwise it writes directly in the template. Important: Do not use spaces in the name of the template.

subroutine savetoexcel(string %groupresult, string %filemane,  string %duplicate)

  if %duplicate = "YES" then

    ' Defines the date string
    %date = @addquotes(@strnow("YYYY-MM-DD_HH-MI-SS"))
    '@uiprompt(@addquotes({%date}))

    ' Defines the path of the template (for windows shell)
    %pathfile = ".\..\..\results\"+%filemane
    '@uiprompt(@addquotes(%pathfile))

    ' Defines the path of the duplicated template with the date (for windows shell)
    %pathfile2 = ".\..\..\results\"+{%date}+"_"+%filemane
    ' Defines the command for windows shell: duplicates the template under another name
    %cmd = "copy  "+%pathfile+" "+%pathfile2 
    shell(h) {%cmd}                   

    ' Defines the path of the duplicated template (for the wfsave command of Eviews)
    %pathfile2b = ".\..\..\results\"+{%date}+"_"+%filemane
 
  else
    ' Defines the path of the template (for the wfsave command of Eviews)
    %pathfile2b = ".\..\..\results\"+%filemane

  endif 

  ' Copy each group of the list in a separate sheet of the (eventually duplicated) template
  for %sheet {%groupresult}
    wfsave(type=excelxml, mode=update) {%pathfile2b} range={%sheet}!a1 byrow @keep {%sheet} @smpl {%baseyear} @last
  next
endsub
' ============================================================================
' ============================================================================
' ==============    INTERPOLATE      =========================================
' ============================================================================
' This subroutine interpolates a list of series %list_na (syntax: "SER1 SER2 SER3") with missing data. At least the baseyear and last date should be available. The subroutines (1) creates a backup of the series computing its steady state calibration; (2) interpolates the series (_int); (3) saves the incomplete series (_na); (4) replaces the series with the interpolated trajectory.

subroutine interpolate(string %list_na)
    
    smpl @all
    ' Interpolate all the variables in the list
    for  %series {%list_na}
      ' Backup of the steady state calibration of the series
      if @elem({%series}, %baseyear) <> 0 then
        'series {%series}_bckp = @elem({%series}, {%baseyear}) * (@elem({%series}, {%baseyear})/@elem({%series}(-1), {%baseyear})) ^ (@year - {%baseyear})
      else
        series {%series}_bckp = 0
      endif
      ' Interpolation of the series
      if @elem({%series}, %baseyear) > 0 then
        {%series}.ipolate(type=lcr) {%series}_int
        ' lcr (log-Catmull-Rom spline) provide the best interpolation: the most stable growth rate
        ' lcr is equivalent to lcs (log-cardinal spline) with tension=0.0 :
        ' {%series}.ipolate(type=lcs, tension=0.0) {%series}_int
      else
         {%series}.ipolate(type=cr) {%series}_int
      endif
      ' Saves the incomplete series
      series {%series}_na = {%series}

      ' Replaces the series with the interpolated trajectory
      {%series} = {%series}_int
    next 
endsub
subroutine interpolate_period(string %list_na, string %firstyear, string %lastyear)
    
    smpl {%firstyear} {%lastyear}
    ' Interpolate all the variables in the list
    for  %series {%list_na}

      ' Interpolation of the series
      if @elem({%series}, %firstyear) > 0 then
        {%series}.ipolate(type=lcr) {%series}_int
        ' lcr (log-Catmull-Rom spline) provide the best interpolation: the most stable growth rate
        ' lcr is equivalent to lcs (log-cardinal spline) with tension=0.0 :
        ' {%series}.ipolate(type=lcs, tension=0.0) {%series}_int
      else
         {%series}.ipolate(type=cr) {%series}_int
      endif

      ' Saves the incomplete series
      series {%series}_na = {%series}
      ' Replaces the series with the interpolated trajectory
      {%series} = {%series}_int

    next 

endsub




' ============================================================================
' ============================================================================
' ==============    CHECK ADDFACTOR  =========================================
' ============================================================================

' This subroutine check if the model is correctly calibrated at the baseyear.
subroutine checkaddfactor(string %model,scalar !threshold)
statusline Creating Add factors and checking if they are different from 0 at baseyear.
' Put add factors to all equations
smpl {%baseyear} {%baseyear}
{%modelname}.addassign @all
' Set add factor values so that the equation has no residual when evaluated at actuals
{%modelname}.addinit(v=n) @all
' Make the list of all endogenous variables
%endo = {%modelname}.@endoglist

' Initialisation of the list of imballanced equations
%imbalance = ""
' Checking and listing the equations with non zero addfactors
for %var {%endo} 
  if @abs(@elem({%var}_a, %baseyear)) > !threshold then
    %imbalance = %imbalance +" "+%var+"_a"
  endif
next 

' Result messages
shell(h) rundll32 user32.dll,MessageBeep
if @str(@wcount(%imbalance)) > 0 then
  scalar answer = @uiprompt("WARNING: NON ZERO ADD FACTORS AT BASEYEAR: "+@str(@wcount(%imbalance))+" equations have a calibration imballance higher than "+@str(!threshold)+". Would you like see them and abord?", "YN")
  if answer = 1 then
      smpl {%baseyear} {%baseyear}
      show {%imbalance}
      stop
  endif
else
 @uiprompt("CHECK ADD FACTORS OK !")
endif

smpl @all
endsub




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


subroutine run_standard(string %scenario_list, string %iso3, scalar !excel)

  call standard_backup

  for %scenario {%scenario_list}
    %index = @str(@wfind(%scenario_list, %scenario))
    %scenario_name = "Scenario" + %scenario
    {%modelname}.scenario(n, a={%index}) %scenario_name
    call standard_shock(%scenario)
    call solvemodel(%solveopt)
    %grp = "Results_" + %scenario + "_" + %iso3
    '%grp_out = "Results" + %scenario
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
  series DSOC_BENF_VAL_bckp = DSOC_BENF_VAL

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

  series PWD_ccoi_bckp = PWD_ccoi

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
  DSOC_BENF_VAL = DSOC_BENF_VAL_bckp

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

  series PWD_ccoi = PWD_ccoi_bckp

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

    EXPG = EXPG + 0.54 * 0.01 * @elem(GDP, %baseyear)
    DSOC_BENF_VAL = 0.46 * 0.01 * @elem(GDP,%baseyear)

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

  PWD_ccoi = PWD_ccoi * 1.1


  endif

  ' 1 GDP point increase of carbon tax
  if @lower(%shock) = "ct1" then

 ' REDIS_CT_LS = 1
 ' REDIS_CT_RRSC = 1 - REDIS_CT_LS

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
' ============================================================================
' ================= STANDARD OUTPUTS =========================================
' ============================================================================


subroutine standard_outputs(string %scenario, string %grp_name, string %index)

  smpl {%baseyear}+1 {%baseyear}+1 {%baseyear}+2 {%baseyear}+2 {%baseyear}+3 {%baseyear}+3 {%baseyear}+5 {%baseyear}+5 {%baseyear}+10 {%baseyear}+10 {%baseyear}+150 {%baseyear}+150
  group {%grp_name} 100*(GDP_{%index}/GDP_0-1) 100*(CH_{%index}/CH_0-1) 100*(I_{%index}/I_0-1) 100*(X_{%index}/X_0-1) 100*(M_{%index}/M_0-1) 100*((DISPINC_AT_VAL_{%index}/PCH_{%index})/(DISPINC_AT_VAL_0/PCH_0)-1) 100*(RSAV_H_VAL_{%index}-RSAV_H_VAL_0) 100*(PCH_{%index}/PCH_0-1) 100*(PY_{%index}/PY_0-1)  100*(PVA_{%index}/PVA_0-1) 100*(PCI_{%index}/PCI_0-1) 100*(PX_{%index}/PCI_0-1) 100*(PM_{%index}/PCI_0-1) 100*(W_{%index}/W_0-1) 100*((C_L_{%index}/PVA_{%index})/(C_L_0/PVA_0)-1) (F_L_{%index}-F_L_0) 100*(UnR_{%index}-UnR_0) 100*(RBal_Trade_VAL_{%index}-RBal_Trade_VAL_0) 100*(RBal_G_Prim_VAL_{%index}-RBal_G_Prim_VAL_0)
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


'==============================================================================
'==============================================================================

subroutine run_euro(string %iso3)

  ' ***********************
  ' Create the Workfile
  %wfname = ".\..\..\results\"+%modelname+"_"+%iso3
  wfcreate(wf=%wfname, page=%modelname) {%freq} {%firstdate} {%lastdate}

  call EViews_lists
  call R_lists

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
    {%modelname}.series ..\model\lists parameters R_calibration_{%iso3}

    ' Export all variables to a csv file (used by the external compiler)
    call export_all_to_csv

    {%modelname}.series round0 Prices_data SU_data Special_data Other_data Exception_taxes_prices_data Exception_NestedCES_data Exception_ConsumerNested_data Exception_Other_data

    ' Export all variables to a csv file (used by the external compiler)
    call export_all_to_csv


    ' Load the model specification from the model/ folder
    {%modelname}.load blocks

    ' Put add factors to all equations
    smpl %firstdate %baseyear

    smpl @all

  else

    ' If the data are already initailized as a Workfile with the option  %load = ""
    ' Load the data
    wfopen {%wfname}    ' Load workfile if already created (the workfile should be in ThreeME_V3\results\)

  endif

  smpl @all

  ' Clean up results folder
  %cmd = @left(@linepath, 2) + " & cd " + @addquotes(@linepath) + " & del /Q ..\..\results\*.txt"

  '************************************************
  '*********** SOLVE SCENARIOS ********************
  '************************************************
  !scenario = 0

  ' ***************************************
  ' Call here the subroutine you want to use to solve the shock

  call run_scenario("baseline")

  call run_standard("EXR10", %iso3, 0)
  'call run_standard("EXR10 EXPG1 RSSC1 INCT1 VAT1 WD1 FF10 CT1", 1) ' Option: 1 for result in excel template; 0 only scenario run

  ' *******************
  ' Error reporting

  string a_errors="Number of errors: "+@str(@errorcount)

  !error_count = @errorcount
  if !error_count > 0 then
    logmode all
    logsave errors
  endif

endsub


