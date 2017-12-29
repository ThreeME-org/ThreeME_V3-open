' The subroutines in this file set up the standard shocks used for comparing
' the model behaviour with other models.
'
' Many of these shocks amount to 1% of baseline GDP, hence they must be calculated
' from an ex-ante baseline run of the model. These subroutine should therefore be
' run after the baseline has been solved.

subroutine standard_backup()

  smpl @all
  series PWD_22_bckp = PWD_22
  series PWD_24_bckp = PWD_22
  series Ttco_bckp = Ttco
  series Shock_TCSE_bckp = Shock_TCSE
  series TENERTD_23_bckp = TENERTD_23
  series TENERTM_23_bckp = TENERTM_23

  for %c 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
    series TVATD_{%c}_bckp = TVATD_{%c}
    series TVATM_{%c}_bckp = TVATM_{%c}
    series TVATDOTH_{%c}_bckp = TVATDOTH_{%c}
    series TVATMOTH_{%c}_bckp = TVATMOTH_{%c}
  next

  IMP_BUD_20 = 0

endsub

subroutine standard_restore_backup()

  smpl @all
  PWD_22 = PWD_22_bckp
  PWD_24 = PWD_22_bckp
  Ttco = Ttco_bckp
  Shock_TCSE = Shock_TCSE_bckp
  TENERTD_23 = TENERTD_23_bckp
  TENERTM_23 = TENERTM_23_bckp

  for %c 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
    TVATD_{%c} = TVATD_{%c}_bckp
    TVATM_{%c} = TVATM_{%c}_bckp
    TVATDOTH_{%c} = TVATDOTH_{%c}_bckp
    TVATMOTH_{%c} = TVATMOTH_{%c}_bckp
  next

  IMP_BUD_20 = 0

endsub

subroutine standard_shock(string %shock)

  call standard_restore_backup

  smpl 2007 @last

  ' 10% increase of fossil fuel prices
  if @lower(%shock) = "ff10" then

    ' PWD_22 = PWD_22 * 0.66
    ' PWD_24 = PWD_22 * 0.66
    PWD_22 = PWD_22 * 1.1
    PWD_24 = PWD_22 * 1.1

  endif

  ' 100% increase of fossil fuel prices
  if @lower(%shock) = "ff100" then

    PWD_22 = PWD_22 * 2
    PWD_24 = PWD_22 * 2

  endif

  ' Increase of the carbon tax by 1% of GDP
  if @lower(%shock) = "co2" then

    TTco = Ttco + 0.01 * PGDP_0 * GDP_0 / EMS_TOT
    ' No redistribution
    Phi_Tco_H01 = 0
    Phi_Tco_ETS = 0
    Phi_Tco_NETS = 0

  endif

  ' Reduction of social contributions paid by the employer by 1% of GDP
  if @lower(%shock) = "cse" then

    Shock_TCSE = -0.01 * PGDP_0 * GDP_0 / (L_S_0 * W_S_0)

  endif

  ' Increase of the electricity tax by 1% of GDP
  if @lower(%shock) = "elec" then

    TENERTD_23 = TENERTD_23 + 0.01 * GDP_0 / YQ_23_0
    ' TENERTM_23 = TENERTM_23 + 0.01 * GDP_0 / M_23_0

  endif

  ' Increase of VAT by 1% of GDP
  if @lower(%shock) = "vat" then

    series VATratio = (PVAT_0 * VAT_0 + 0.01 * PGDP_0 * GDP_0) / (PVAT_0 * VAT_0)

    for %c 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
      TVATD_{%c} = TVATD_{%c} * VATratio
      TVATM_{%c} = TVATM_{%c} * VATratio
      TVATDOTH_{%c} = TVATDOTH_{%c} * VATratio
      TVATMOTH_{%c} = TVATMOTH_{%c} * VATratio
    next

  endif

  ' Increase of IAPU by 1% of GDP
  if @lower(%shock) = "iapu" then

    smpl 2007 2007
    IMP_BUD_20 = 0.01

  endif

  smpl @all

endsub
