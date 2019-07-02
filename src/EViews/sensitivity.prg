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
  'call setThreeMe("TV_NAIRU", 0)
endif
if !wage_eq = 1 then
  call setThreeMe("RHO_W_U[s]", 0.2)
  call setThreeMe("RHO_W_DU[s]", 0.6)
  'call setThreeMe("TV_NAIRU", 0)
endif
if !wage_eq = 2 then
  call setThreeMe("RHO_W_U[s]", 0.2)
  call setThreeMe("RHO_W_DU[s]", 0.6)
  'call setThreeMe("TV_NAIRU", 1)
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
