%baseyear = "2010"
%list_sec = "sAZ sDE sC1 sC2 sC3 sC4 sC5 sFZ sGZ sHZ sIZ sJZ sKZ sLZ sMN sOQ sRU"    
%list_com = "cAZ cDE cC1 cC2 cC3 cC4 cC5 cFZ cGZ cHZ cIZ cJZ cKZ cLZ cMN cOQ cRU"
%list_GHG = "CO2 CH4 N2O SF6 HFC PFC"

    smpl {%baseyear} @last  
    %index = "2"
    group Macro 100*(GDP_{%index}/GDP_0-1) 100*(CH_{%index}/CH_0-1) 100*(I_{%index}/I_0-1) 100*(X_{%index}/X_0-1) 100*(M_{%index}/M_0-1) 100*((DISPINC_AT_VAL_{%index}/PCH_{%index})/(DISPINC_AT_VAL_0/PCH_0)-1) 100*(RSAV_H_VAL_{%index}-RSAV_H_VAL_0) 100*(PCH_{%index}/PCH_0-1) 100*(PY_{%index}/PY_0-1)  100*(PVA_{%index}/PVA_0-1) 100*(PCI_{%index}/PCI_0-1) 100*(PX_{%index}/PX_0-1) 100*(PM_{%index}/PM_0-1) 100*(W_{%index}/W_0-1) 100*((C_L_{%index}/PVA_{%index})/(C_L_0/PVA_0)-1) (F_L_{%index}-F_L_0) 100*(UnR_{%index}-UnR_0) 100*(RBal_Trade_VAL_{%index}-RBal_Trade_VAL_0) 100*(RBal_G_Prim_VAL_{%index}-RBal_G_Prim_VAL_0) 100*(RDEBT_G_VAL_{%index}-RDEBT_G_VAL_0) 100*(EMS_{%index}/EMS_0-1) 100*(CH_0+G_0)/GDP_0*((CH_{%index}+G_{%index})/(CH_0+G_0)-1) 100*I_0/GDP_0*(I_{%index}/I_0-1) 100*(X_0-M_0)/GDP_0*((X_{%index}-M_{%index})/(X_0-M_0)-1) 100*DS_0/GDP_0*(DS_{%index}/DS_0-1)

    ' Sectorial results
    %report_sect = "100*(Y_2/Y_0-1) 100*(F_L_2/F_L_0-1) (F_L_2-F_L_0)  100*(EMS_CI_CO2_2/EMS_CI_CO2_0-1)" 
    
    for %s {%list_sec}
       %report_sect = %report_sect + "100*(Y_"+%s+"_2/Y_"+%s+"_0-1) "  
    next

    for %s {%list_sec}
       %report_sect = %report_sect + "100*(F_L_"+%s+"_2/F_L_"+%s+"_0-1) "  
    next

    for %s {%list_sec}
       %report_sect = %report_sect + "(F_L_"+%s+"_2-F_L_"+%s+"_0) "  
    next

    for %s {%list_sec}
       %report_sect = %report_sect + "100*(VA_"+%s+"_2/VA_"+%s+"_0-1) "  
    next


    for %s {%list_sec}
       %report_sect = %report_sect + "100*(I_"+%s+"_2/I_"+%s+"_0-1) "  
    next

    group Sectors {%report_sect}

    ' GHG results

    %report_ghg =               "EMS_2 EMS_0 (EMS_2 - EMS_0) 100*(EMS_2/EMS_0-1) "
    %report_ghg = %report_ghg + "EMS_CI_2 EMS_CI_0 (EMS_CI_2 - EMS_CI_0) 100*(EMS_CI_2/EMS_CI_0-1) "
    %report_ghg = %report_ghg + "EMS_MAT_2 EMS_MAT_0 (EMS_MAT_2 - EMS_MAT_0) 100*(EMS_MAT_2/EMS_MAT_0-1) "
    %report_ghg = %report_ghg + "EMS_Y_2 EMS_Y_0 (EMS_Y_2 - EMS_Y_0) 100*(EMS_Y_2/EMS_Y_0-1) "
    %report_ghg = %report_ghg + "EMS_CH_2 EMS_CH_0 (EMS_CH_2 - EMS_CH_0) 100*(EMS_CH_2/EMS_CH_0-1) "

    for %ghg {%list_GHG}
       %report_ghg = %report_ghg + "EMS_"+%ghg+"_2 EMS_"+%ghg+"_0 "  
       %report_ghg = %report_ghg + "(EMS_"+%ghg+"_2 - EMS_"+%ghg+"_0) "  
       %report_ghg = %report_ghg + "100*(EMS_"+%ghg+"_2/EMS_"+%ghg+"_0-1) "
    next

    for %s {%list_sec}
       %report_ghg = %report_ghg + "EMS_CI_"+%s+"_2 EMS_CI_"+%s+"_0 "  
       %report_ghg = %report_ghg + "(EMS_CI_"+%s+"_2 - EMS_CI_"+%s+"_0) "  
       %report_ghg = %report_ghg + "100*(EMS_CI_"+%s+"_2/EMS_CI_"+%s+"_0-1) "
    next

    for %c {%list_com}
       if EMS_{%c} <> 0 then
         %report_ghg = %report_ghg + "EMS_"+%c+"_2 EMS_"+%c+"_0 "  
         %report_ghg = %report_ghg + "(EMS_"+%c+"_2 - EMS_"+%c+"_0) "  
         %report_ghg = %report_ghg + "100*(EMS_"+%c+"_2/EMS_"+%c+"_0-1) "
       endif
    next

    for %c {%list_com}
       if EMS_CI_{%c} <> 0 then
         %report_ghg = %report_ghg + "EMS_CI_"+%c+"_2 EMS_CI_"+%c+"_0 "  
         %report_ghg = %report_ghg + "(EMS_CI_"+%c+"_2 - EMS_CI_"+%c+"_0) "  
         %report_ghg = %report_ghg + "100*(EMS_CI_"+%c+"_2/EMS_CI_"+%c+"_0-1) "
       endif
    next

    for %c {%list_com}
       if EMS_CH_{%c} <> 0 then
         %report_ghg = %report_ghg + "EMS_CH_"+%c+"_2 EMS_CH_"+%c+"_0 "  
         %report_ghg = %report_ghg + "(EMS_CH_"+%c+"_2 - EMS_CH_"+%c+"_0) "  
         %report_ghg = %report_ghg + "100*(EMS_CH_"+%c+"_2/EMS_CH_"+%c+"_0-1) "
       endif
    next

    group GHG {%report_ghg}


show Macro
show Sectors
show GHG


