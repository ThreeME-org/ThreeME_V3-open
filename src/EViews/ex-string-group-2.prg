' Ex SANS creation d'une nouvelle série
%baseyear = "2015"
%list_ener_class = "CA CB CC CD CE CF CG"
%list_sec_ELE =  "senu seoi sega seco sewi seso sehy sech seot"
%list_com_E = "ccoa ccoi cfut cfuh cgas cele chea cbio cote"
%list_com = "cagr cfor cfoo cveh cgla cpap cche cpla cmet cigo ccgo ccon crai croa cwat cair cpri cpub cmin ccoa ccoi cfut cfuh cgas cele chea cbio cote"
%list_GHG = "CO2 CH4 N2O SF6 HFC PFC"
%list_sec_AGREG = "agrf ind trsp ser trsf ele"
%list_sec = "sagr sfor sfoo sveh sgla spap sche spla smet sigo scgo scon srai sroa swat sair spri spub smin soil sbfu sgas sbga senu seoi sega seco sewi seso sehy sech seot"
' Send results to Excel 
  
%macro = " 100*(GDP_2/GDP_0-1) 100*((VA_2-VA_spub_2)/(VA_0-VA_spub_0)-1) 100*((CH_2-CH_0)/GDP_0) 100*((CH_cveh_2-CH_cveh_0)/GDP_0) 100*((G_2-G_0)/GDP_0) 100*((I_2-I_0)/GDP_0) 100*((DS_2-DS_0)/GDP_0) 100*((X_2-X_0)/GDP_0)-100*((M_2-M_0)/GDP_0) 100*((CH_2-CH_ccon_2-(CH_0-CH_ccon_0))/GDP_0) 100*((I_2+CH_ccon_2-(I_0+CH_ccon_0))/GDP_0) 100*((I_2-IA_spub_2-(I_0-IA_spub_0))/GDP_0) 100*((CH_ccon_2-CH_ccon_0)/GDP_0) 100*((IA_spub_2-IA_spub_0)/GDP_0) 100*(CH_2/CH_0-1) 100*(CH_cveh_2/CH_cveh_0-1) 100*(G_2/G_0-1) 100*(I_2/I_0-1) 100*((I_2-IA_spub_2)/(I_0-IA_spub_0)-1) 100*(X_2/X_0-1) 100*(M_2/M_0-1) 100*((CH_2-CH_ccon_2)/(CH_0-CH_ccon_0)-1) 100*((I_2+CH_ccon_2)/(I_0+CH_ccon_0)-1) 100*((CH_ccon_2)/(CH_ccon_0)-1) 100*((IA_spub_2)/(IA_spub_0)-1) 100*(DISPINC_AT_VAL_2/DISPINC_AT_VAL_0-1) 100*(DISPINC_AT_VAL_2/DISPINC_AT_VAL_0-1)-100*(PCH_2/PCH_0-1) 100*(DISPINC_AT_VAL_2/DISPINC_AT_VAL_0-1)-100*((F_L_2/F_L_0)-1) 100*(DISPINC_AT_VAL_2/DISPINC_AT_VAL_0-1)-100*((F_L_2/F_L_0)-1)-100*(PCH_2/PCH_0-1) 100*(MPS_n_2-MPS_n_0) 100*(PCH_2/PCH_0-1) 100*(PY_2/PY_0-1) 100*(PX_2/PX_0-1) 100*(PM_2/PM_0-1) 100*(W_2/W_0-1) 100*((W_2/PCH_2)/(W_0/PCH_0)-1) 100*(C_L_2/C_L_0-1) 100*((C_L_2/PVA_2)/(C_L_0/PVA_0)-1) ((F_L_2/F_L_0)-1)*100 F_L_2-F_L_0 100*(UnR_2-UnR_0) 100*(Bal_Trade_VAL_2/(GDP_2*PGDP_2)-Bal_Trade_VAL_0/(GDP_0*PGDP_0)) ENER_BILL 100*((-Bal_G_prim_VAL_2)/(GDP_2*PGDP_2)-(-Bal_G_prim_VAL_0)/(GDP_0*PGDP_0)) 100*((-Bal_G_tot_VAL_2)/(GDP_2*PGDP_2)-(-Bal_G_tot_VAL_0)/(GDP_0*PGDP_0)) (DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_0/(PGDP_0*GDP_0))*100 "    
 
group Macro {%macro}    
show Macro


