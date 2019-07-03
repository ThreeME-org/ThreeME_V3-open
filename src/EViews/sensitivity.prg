subroutine sensitivity(scalar !redis_ls, scalar !wage_eq, scalar !flex, scalar !expo)

if !redis_ls = 0 then
  call setThreeMe("REDIS_CT_LS", 0)
endif
if !redis_ls = 1 then
    call setThreeMe("REDIS_CT_LS", 1)
endif

if !wage_eq = 0 then
  call setThreeMe("RHO_W_U[s]", 0)
  call setThreeMe("RHO_W_DU[s]", 0.6)
  'call setThreeMe("RHO_Cons_U[s]", 0)
endif
if !wage_eq = 1 then
  call setThreeMe("RHO_W_U[s]", 0.2)
  call setThreeMe("RHO_W_DU[s]", 0.6)
  'call setThreeMe("RHO_Cons_U[s]", 0)
endif
if !wage_eq = 2 then
  call setThreeMe("RHO_W_U[s]", 0.2)
  call setThreeMe("RHO_W_DU[s]", 0.6)
  'call setThreeMe("RHO_Cons_U[s]", 0.5)
endif

if !flex = 0 then
  call setThreeMe("ES_NEST_K_E[s]", 0.2)
  call setThreeMe("ES_NRJ[ce, cee, s]", 0.2)
  call setThreeMe("ES_TRSP[ct, ctt, s]", 0.2)
  call setThreeMe("ES_MGPD_crai_croa[c]", 0.2)
  call setThreeMe("ES_MGPD_croa_crai[c]", 0.2)
  call setThreeMe("ES_MGPM_crai_croa[c]", 0.2)
  call setThreeMe("ES_MGPM_croa_crai[c]", 0.2)
  call setThreeMe("ES_HOUS_INV_ENER", 0.2)
  call setThreeMe("ES_HOUS_ENER", 0.2)
  call setThreeMe("ES_CHTRSP", 0.2)
  call setThreeMe("ES_TRSP_INV_ENER", 0.2)
  call setThreeMe("ES_TRSP_ENER", 0.2)
endif
if !flex = 1 then
  call setThreeMe("ES_NEST_K_E[s]", 2)
  call setThreeMe("ES_NRJ[ce, cee, s]", 2)
  call setThreeMe("ES_TRSP[ct, ctt, s]", 2)
  call setThreeMe("ES_MGPD_crai_croa[c]", 2)
  call setThreeMe("ES_MGPD_croa_crai[c]", 2)
  call setThreeMe("ES_MGPM_crai_croa[c]", 2)
  call setThreeMe("ES_MGPM_croa_crai[c]", 2)
  call setThreeMe("ES_HOUS_INV_ENER", 2)
  call setThreeMe("ES_HOUS_ENER", 2)
  call setThreeMe("ES_CHTRSP", 2)
  call setThreeMe("ES_TRSP_INV_ENER", 2)
  call setThreeMe("ES_TRSP_ENER", 2)
endif

if !expo = 0 then
  call setThreeMe("ES_CHM[c]", 0.6)
  call setThreeMe("ES_GM[c]", 0.6)
  call setThreeMe("ES_IM[c]", 0.6)
  call setThreeMe("ES_XM[c]", 0.6)
  call setThreeMe("ES_CIM[c,s]", 0.6)
  call setThreeMe("ES_IM[c,s]", 0.6)
  call setThreeMe("ES_MGSM[m]", 0.6)
  call setThreeMe("ES_X[c]", 0.6)

  call setThreeMe("ES_CHM[ce]", 0)
  call setThreeMe("ES_GM[ce]", 0)
  call setThreeMe("ES_IM[ce]", 0)
  call setThreeMe("ES_XM[ce]", 0)
  call setThreeMe("ES_CIM[ce, s]", 0)
  call setThreeMe("ES_IM[ce, s]", 0)
endif

if !expo = 1 then
  call setThreeMe("ES_CHM[c]", 2)
  call setThreeMe("ES_GM[c]", 2)
  call setThreeMe("ES_IM[c]", 2)
  call setThreeMe("ES_XM[c]", 2)
  call setThreeMe("ES_CIM[c,s]", 2)
  call setThreeMe("ES_IM[c,s]", 2)
  call setThreeMe("ES_MGSM[m]", 2)
  call setThreeMe("ES_X[c]", 2)

  call setThreeMe("ES_CHM[ce]", 0)
  call setThreeMe("ES_GM[ce]", 0)
  call setThreeMe("ES_IM[ce]", 0)
  call setThreeMe("ES_XM[ce]", 0)
  call setThreeMe("ES_CIM[ce, s]", 0)
  call setThreeMe("ES_IM[ce, s]", 0)
endif

endsub



subroutine sensitivity_results(scalar !iteration, scalar !max_iteration)
  group Results redis_ls wage_eq flex expo 100*(GDP_1/GDP_0-1) (F_L_1-F_L_0) 100*(EMS_1/EMS_0-1) 100*(CH_1/CH_0-1) 100*(I_1/I_0-1) 100*(X_1/X_0-1) 100*(M_1/M_0-1) 100*((DISPINC_AT_VAL_1/PCH_1)/(DISPINC_AT_VAL_0/PCH_0)-1) 100*(RSAV_H_VAL_1-RSAV_H_VAL_0) 100*(PCH_1/PCH_0-1) 100*(PY_1/PY_0-1)  100*(PVA_1/PVA_0-1) 100*(PCI_1/PCI_0-1) 100*(PX_1/PCI_0-1) 100*(PM_1/PCI_0-1) 100*(W_1/W_0-1) 100*((C_L_1/PVA_1)/(C_L_0/PVA_0)-1) 100*(UnR_1-UnR_0) 100*(RBal_Trade_VAL_1-RBal_Trade_VAL_0) 100*(RBal_G_Prim_VAL_1-RBal_G_Prim_VAL_0)

  %address = """Raw!a" + @str(1 + (!max_iteration - !iteration - 1) * 51) + """"
  wfsave(type=excelxml, mode=update) ".\..\..\results\Sensitivity.xlsx" range={%address} @keep Results @smpl {%baseyear} {%lastdate}
endsub