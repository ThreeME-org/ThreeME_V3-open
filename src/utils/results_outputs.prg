subroutine output_template(string %scenario)

  delete(noerr) output_list
  text output_list
  output_list.append(file) .\output_list.txt

  %series_list = ""

  for !i = 1 to output_list.@linecount
    %line = output_list.@line(!i)
    %series_name = @trim(@mid(%line, 1, @instr(%line, "=") - 1))
    %series_formula = @trim(@mid(%line, @instr(%line, "=") + 1))
    if @length(%series_name) > 0 then
      series {%series_name} = {%series_formula}
      %series_list = %series_list + " " + %series_name
    endif
  next

  smpl @all
  group results {%series_list}
  freeze(mode = overwrite, tab_results) results
  tab_results.save(t=txt) .\..\..\results\{%scenario}.txt

endsub

subroutine standard_outputs(string %grp_name, string %index)

  smpl 2007 2007 2008 2008 2009 2009 2011 2011 2016 2016 2041 2041
  group {%grp_name} 100*(GDP_{%index}/GDP_0-1) 100*(VA_19_{%index}/VA_19_0-1) 100*(CH_{%index}/CH_0-1) 100*(IA_{%index}/IA_0-1) 100*((IA_{%index}-IA_20_{%index})/(IA_0-IA_20_0)-1) 100*(X_{%index}/X_0-1) 100*(M_{%index}/M_0-1) 100*(PCH_{%index}/PCH_0-1) 100*(PY_{%index}/PY_0-1) 100*((W_{%index}/PCH_{%index})/(W_0/PCH_0)-1) 100*((CL_{%index}/PVA_{%index})/(CL_0/PVA_0)-1) L_{%index}-L_0 100*(UNR_TOT_{%index}-UNR_TOT_0) 100*((PX_{%index}*X_{%index}-PM_{%index}*M_{%index})/(PGDP_{%index}*GDP_{%index})-(PX_0*X_0-PM_0*M_0)/(PGDP_0*GDP_0)) 100*(EMS_TOT_{%index}/EMS_TOT_0-1)
  freeze(mode = overwrite, tab_results) {%grp_name}
  tab_results.save(t=txt) .\..\..\results\standard\{%grp_name}.txt

  smpl @all
  group {%grp_name}_all 100*(GDP_{%index}/GDP_0-1) 100*(VA_19_{%index}/VA_19_0-1) 100*(CH_{%index}/CH_0-1) 100*(IA_{%index}/IA_0-1) 100*((IA_{%index}-IA_20_{%index})/(IA_0-IA_20_0)-1) 100*(X_{%index}/X_0-1) 100*(M_{%index}/M_0-1) 100*(PCH_{%index}/PCH_0-1) 100*(PY_{%index}/PY_0-1) 100*((W_{%index}/PCH_{%index})/(W_0/PCH_0)-1) 100*((CL_{%index}/PVA_{%index})/(CL_0/PVA_0)-1) L_{%index}-L_0 100*(UNR_TOT_{%index}-UNR_TOT_0) 100*((PX_{%index}*X_{%index}-PM_{%index}*M_{%index})/(PGDP_{%index}*GDP_{%index})-(PX_0*X_0-PM_0*M_0)/(PGDP_0*GDP_0)) 100*(EMS_TOT_{%index}/EMS_TOT_0-1)
  freeze(mode = overwrite, tab_results) {%grp_name}_all
  tab_results.save(t=txt) .\..\..\results\standard\{%grp_name}_all.txt

  ' smpl @all
  ' group all_results *
  ' freeze(mode = overwrite, tab_all) all_results
  ' tab_all.save(t=csv) .\..\..\results\all.csv

endsub


subroutine dgec_outputs(scalar !i)

  %i = @str(!i)

  smpl 2006 2006 2010 2010 2015 2015 2020 2020 2025 2025 2030 2030 2035 2035

  group results_scenario_{%i} Q_Mtep_SEC_01_{%i} Q_Mtep_SEC_02_{%i} Q_Mtep_SEC_03_{%i} Q_Mtep_SEC_04_{%i} Q_Mtep_SEC_05_{%i} Q_Mtep_SEC_06_{%i} Q_Mtep_SEC_07_{%i} Q_Mtep_SEC_08_{%i} Q_Mtep_SEC_09_{%i} Q_Mtep_SEC_10_{%i} Q_Mtep_SEC_11_{%i} Q_Mtep_SEC_12_{%i} Q_Mtep_SEC_13_{%i} Q_Mtep_SEC_14_{%i} Q_Mtep_SEC_15_{%i} Q_Mtep_SEC_16_{%i} Q_Mtep_SEC_17_{%i} Q_Mtep_SEC_18_{%i} Q_Mtep_SEC_19_{%i} Q_Mtep_SEC_20_{%i} Q_Mtep_H_Auto_{%i} Q_Mtep_H_Buil_{%i} Q_Mtep_ep_21_{%i} Q_Mtep_ep_2201_{%i} Q_Mtep_ep_2202_{%i} Q_Mtep_ep_2301_{%i} Q_Mtep_ep_2302_{%i} Q_Mtep_ep_2303_{%i} Q_Mtep_ep_2304_{%i} Q_Mtep_ep_2305_{%i} Q_Mtep_ep_2306_{%i} Q_Mtep_ep_2307_{%i} Q_Mtep_ep_2308_{%i} Q_Mtep_ep_2401_{%i} Q_Mtep_ep_2402_{%i} Q_Mtep_ep_2403_{%i} Q_Mtep_ep_2404_{%i} Q_Mtep_ep_2405_{%i} Q_Mtep_ep_2406_{%i}  100*(GDP_{%i}/GDP_0-1) 100*(CH_{%i}/CH_0-1) 100*(I_{%i}/I_0-1) 100*(G_{%i}/G_0-1) 100*(X_{%i}/X_0-1) 100*(M_{%i}/M_0-1) 100*(UNR_TOT_{%i}-UNR_TOT_0) 100*(L_{%i}/L_0-1) 100*(W_{%i}/W_0-1) 100*(@PCHY(PCH_{%i})-@PCHY(PCH_0)) 100*(R_{%i}-R_0) 100*(BF_G_VAL_{%i}/(PGDP_{%i}*GDP_{%i})-BF_G_VAL_0/(PGDP_0*GDP_0)) 100*(DEBT_G_VAL_{%i}/(PGDP_{%i}*GDP_{%i}) - DEBT_G_VAL_0/(PGDP_0*GDP_0)) (100*((PX_{%i}*X_{%i}-PM_{%i}*M_{%i})/(PGDP_{%i}*GDP_{%i}) - (PX_0*X_0-PM_0*M_0)/(PGDP_0*GDP_0))) 100*(GDP_{%i} / @ELEM(GDP_{%i}, %baseyear)) 100*(EMS_TOT_{%i} / @ELEM(EMS_TOT_{%i}, %baseyear)) 1000000*(TTCO_{%i}/PCH_{%i}-TTCO_0/PCH_0) (REC_TCO_{%i}/PCH_{%i}-REC_TCO_0/PCH_0)/1000

  freeze(mode = overwrite, tab_results) results_scenario_{%i}
  tab_results.save(t=txt) .\..\..\results\dgec\results_scenario_{%i}.txt

endsub

subroutine additional_outputs

  smpl @all


  if %save = "yes" and %tabopt = "VAR_MESANGE" then


    %tablename = "tab_macro_2"
    freeze(mode = overwrite, {%tablename}) a_{%tablename}
    show  {%tablename}

    shell if not exist output_{%DC} mkdir output_{%DC}  ' Windows create the output folder if it does not exist
    {%tablename}.save(t=txt) output_{%DC}\{%tablename}_{%DS}.xls

    shell if not exist output_graphs mkdir output_graphs  ' Windows create the output folder if it does not exist

    for %format png 'jpg 'bmp gif
      a_graph_macro_2.setupdate(manual) @all
      a_graph_macro_2.update
      a_graph_macro_2.save(t={%format}, d=500)  output_graphs\allsmpl_graph_macro_2_{%DS}_{%DC} ' Save graph; t=format; d=nb of dots per inch

      if {%lastdate}>{%lastdate_graph} then
        a_graph_macro_2.setupdate(manual) @first {%lastdate_graph}
        a_graph_macro_2.update
        a_graph_macro_2.save(t={%format}, d=500)  output_graphs\{%lastdate_graph}_graph_macro_2_{%DS}_{%DC}  ' Save graph; t=format; d=nb of dots per inch
      endif
    next

    'for %format png 'jpg 'bmp gif                                                                                  ' Save graph; t=format; d=nb of dots per inch
    'a_graph_production2.setupdate(manual) @all
    'a_graph_production2.update
    'a_graph_production2.save(t={%format}, d=500)  output_graphs\allsmpl_graph_production2_{%DS}_{%DC}
    'if {%lastdate}>{%lastdate_graph} then
    'a_graph_production2.setupdate(manual) @first {%lastdate_graph}
    'a_graph_production2.update
    'a_graph_production2.save(t={%format}, d=500)  output_graphs\{%lastdate_graph}_graph_production2_{%DS}_{%DC}                                                                                      ' Save graph; t=format; d=nb of dots per inch
    'endif
    'next

  endif


  if %save = "yes" and %tabopt = "3MEBLOCK" then


    %tablename = "tab_shock"
    freeze(mode = overwrite, {%tablename}) a_{%tablename}
    show  {%tablename}

    shell if not exist output_3ME_{%DC}\output_{%DS} mkdir output_3ME_{%DC}\output_{%DS}  ' Windows create the output folder if it does not exist
    {%tablename}.save(t=txt) output_3ME_{%DC}\output_{%DS}\{%tablename}.xls

    shell if not exist output_graphs mkdir output_graphs  ' Windows create the output folder if it does not exist

    %tablename_2 = "tab_baseline"
    freeze(mode = overwrite, {%tablename_2}) a_{%tablename_2}
    show  {%tablename}

    shell if not exist output_3ME_{%DC}\output_{%DS} mkdir output_3ME_{%DC}\output_{%DS}  ' Windows create the output folder if it does not exist
    {%tablename_2}.save(t=txt) output_3ME_{%DC}\output_{%DS}\{%tablename_2}.xls

    shell if not exist output_graphs mkdir output_graphs  ' Windows create the output folder if it does not exist

    for %format png 'jpg 'bmp gif
      a_graph_macro_2.setupdate(manual) @all
      a_graph_macro_2.update
      a_graph_macro_2.save(t={%format}, d=500)  output_graphs\allsmpl_graph_macro_2_{%DS}_{%DC}

      if {%lastdate}>{%lastdate_graph} then
        a_graph_macro_2.setupdate(manual) @first {%lastdate_graph}
        a_graph_macro_2.update
        a_graph_macro_2.save(t={%format}, d=500)  output_graphs\{%lastdate_graph}_graph_macro_2_{%DS}_{%DC}
      endif
    next

    for %format png 'jpg 'bmp gif
      ' Save graph; t=format; d=nb of dots per inch
      a_graph_production2.setupdate(manual) @all
      a_graph_production2.update
      a_graph_production2.save(t={%format}, d=500)  output_graphs\allsmpl_graph_production2_{%DS}_{%DC}


      if {%lastdate}>{%lastdate_graph} then

        a_graph_production2.setupdate(manual) @first {%lastdate_graph}
        a_graph_production2.update
        a_graph_production2.save(t={%format}, d=500)  output_graphs\{%lastdate_graph}_graph_production2_{%DS}_{%DC}

        ' Save graph; t=format; d=nb of dots per inch
      endif

    next

    for %format png 'jpg 'bmp gif
      ' Save graph; t=format; d=nb of dots per inch
      a_graph_Employment2.setupdate(manual) @all
      a_graph_Employment2.update
      a_graph_Employment2.save(t={%format}, d=500)  output_graphs\allsmpl_graph_Employment2_{%DS}_{%DC}

      if {%lastdate}>{%lastdate_graph} then
        a_graph_Employment2.setupdate(manual) @first {%lastdate_graph}
        a_graph_Employment2.update
        a_graph_Employment2.save(t={%format}, d=500)  output_graphs\{%lastdate_graph}_graph_Employment2_{%DS}_{%DC} ' Save graph; t=format; d=nb of dots per inch
      endif
    next

    for %format png 'jpg 'bmp gif
      ' Save graph; t=format; d=nb of dots per inch
      a_graph_ProductionPrice2.setupdate(manual) @all
      a_graph_ProductionPrice2.update
      a_graph_ProductionPrice2.save(t={%format}, d=500)  output_graphs\allsmpl_graph_ProductionPrice2_{%DS}_{%DC}

      if {%lastdate}>{%lastdate_graph} then
        a_graph_ProductionPrice2.setupdate(manual) @first {%lastdate_graph}
        a_graph_ProductionPrice2.update
        a_graph_ProductionPrice2.save(t={%format}, d=500)  output_graphs\{%lastdate_graph}_graph_ProductionPrice2_{%DS}_{%DC} ' Save graph; t=format; d=nb of dots per inch
      endif
    next

  endif


endsub

' *******************************************************************************************************************************
' ***************************************************** create_seriesresults *****************************************
' *******************************************************************************************************************************
' Re-creates exogeneous series and creates missing series with "_0" and "_2" for including them in tables
subroutine create_seriesresults(string %graphopt)

  if %graphopt="3MEBLOCK" then

    for %seriesname Ttco
      series {%seriesname}_0  =  {%seriesname}
    next

    for %seriesname Ttco PhiY_22_2201 PhiY_22_2202 PhiY_23_2301 PhiY_23_2302 PhiY_23_2303 _
      PhiY_23_2304 PhiY_23_2305 PhiY_23_2306 PhiY_23_2307 PhiY_23_2308 _
      PhiY_24_2401 PhiY_24_2402 PhiY_24_2403 PhiY_24_2404 PhiY_24_2405 PhiY_24_2406
      series {%seriesname}_2  =  {%seriesname}
    next
  endif

endsub

' *******************************************************************************************************************************
' ***************************************************** GRAPH  SUBROUTINE *****************************************
' *******************************************************************************************************************************
' This subroutine makes graph for selected variables
subroutine graph(string %graphopt)


if %graphopt="ADEME" then
  if !scenario = 1 then

      %_x = "_0"    'Scenario to be plotted: "_0" for baseline "_2" for shock

      %TS = "TS_HH"

      graph a_graph_macro_0.line(m) 100*@PCHY(GDP{%_x}) 100*@PCHY(Y{%_x}) 100*@PCHY(VA{%_x}) 100*@PCHY(CH{%_x}) 100*@PCHY(G{%_x}) 100*@PCHY(IA{%_x}) _
      100*@PCHY(X{%_x}) 100*@PCHY(M{%_x}) 100*@PCHY(DISPINC_VAL{%_x}/PCH{%_x}) 100*{%TS}{%_x} 100*@PCHY(PCH{%_x}) _
      100*@PCHY(PYQ{%_x}) 100*@PCHY(PX{%_x}) 100*@PCHY(PM{%_x}) 100*@PCHY(W{%_x}/PCH{%_x}) 100*@PCHY(CL{%_x}/PY{%_x}) _
      100*(UNR_TOT{%_x}) 100*@PCHY(L{%_x}) 100*DC_VAL{%_x}/(PGDP{%_x}*GDP{%_x}) -100*DP_G_VAL{%_x} 100*DEBT_G_VAL{%_x}/(PGDP{%_x}*GDP{%_x}) FISC{%_x} 100*r{%_x}
      show  a_graph_macro_0


      %series = ""
      For %com {%list_com_E_CO2}
       %series  = %series+" 100*EMS_SOU"+%_x+"/@elem(EMS_SOU"+%_x+","+%baseyear+")"
      next

      graph a_graph_ems_0.line(m) 100*@PCHY(EMS_TOT{%_x}) 100*@PCHY(EMS_SEC{%_x}) 100*@PCHY(EMS_DC{%_x}) 100*@PCHY(EMS_HH{%_x}) _
                                  100*EMS_TOT{%_x}/@elem(EMS_TOT{%_x},%baseyear) 100*EMS_SEC{%_x}/@elem(EMS_SEC{%_x},%baseyear) _
                                  100*EMS_DC{%_x}/@elem(EMS_DC{%_x},%baseyear) 100*EMS_HH{%_x}/@elem(EMS_HH{%_x},%baseyear) _
                                  {%series}

      show  a_graph_ems_0

      '-----------***Graph Buildings***------------------'
      %series = ""
      For %cl {%list_buil_class}
          %series  = %series+" BUIL_"+%cl+%_x
      next

      graph a_graph_buil_0.line(m) {%series} BUIL{%_x}
      show  a_graph_buil_0

      %series = ""
      For %cl {%list_ener_class}
          %series  = %series+" 100*BUIL_"+%cl+%_x+"/BUIL"+%_x
      next

      graph a_graph_builshare_0.line(m) {%series}
      show  a_graph_builshare_0



      %series = ""
      For %h {%list_household}
      	For %ecl CB CC CD CE CF CG
          		%series  = %series+" Payback_REHAB_"+%h+"_"+%ecl+%_x+" 100*tau_REHAB_"+%h+"_"+%ecl+%_x
      	next
      next

      graph a_graph_Payback_0.line(m) {%series}
      show  a_graph_Payback_0

      '-----------***Graph Auto***------------------'
      %series = ""
      For %cl {%list_buil_class}
          %series  = %series+" AUTO_"+%cl+%_x
      next

      graph a_graph_AUTO_0.line(m) {%series} AUTO{%_x}
      show  a_graph_AUTO_0

      %series = ""
      For %cl {%list_ener_class}
          %series  = %series+" 100*AUTO_"+%cl+%_x+"/AUTO"+%_x
      next

      graph a_graph_AUTOshare_0.line(m) {%series}
      show  a_graph_AUTOshare_0


  endif

  if !scenario = 2 then

      %_x = "_2"    'Scenario to be plotted: "_0" for baseline "_2" for shock

      ' freeze graph a_graph_macro_2.line(m) 100*@PCHY(GDP_2) 100*@PCHY(Y_2) 100*@PCHY(VA_2) 100*@PCHY(CH_2) 100*@PCHY(G_2) 100*@PCHY(IA_2) _
      ' freeze 100*@PCHY(X_2) 100*@PCHY(M_2) 100*@PCHY(DISPINC_VAL_2/PCH_2) 100*{%TS}_2 100*@PCHY(PCH_2) _
      ' freeze 100*@PCHY(PYQ_2) 100*@PCHY(PX_2) 100*@PCHY(PM_2) 100*@PCHY(W_2/PCH_2) 100*@PCHY(CL_2/PY_2) _
      ' freeze 100*(UNR_TOT_2) 100*@PCHY(L_2) 100*DC_VAL_2/(PGDP_2*GDP_2) -100*DP_G_VAL_2 100*DEBT_G_VAL_2/(PGDP_2*GDP_2) FISC_2 100*r_2
      ' freeze show  a_graph_macro_2

      graph a_graph_macro_2L.line(m) 100*(GDP_2/GDP_0-1) 100*((VA_2-VA_20_2)/(VA_0-VA_20_0)-1) 100*(CH_2/CH_0-1) 100*(G_2/G_0-1) 100*(IA_2/IA_0-1) _
      100*(IA_19_2/IA_19_0-1) 100*(X_2/X_0-1) 100*(M_2/M_0-1) 100*(DISPINC_VAL_2/PCH_2/(DISPINC_VAL_0/PCH_0)-1) _
      100*({%TS}_2-{%TS}_0) 100*(PCH_2/PCH_0-1) 100*(PYQ_2/PYQ_0-1) 100*(PX_2/PX_0-1) 100*(PM_2/PM_0-1) _
      100*((W_2/PCH_2)/(W_0/PCH_0)-1) 100*((CL_2/PY_2)/(CL_0/PY_0)-1) L_2-L_0 100*(UNR_TOT_2-UNR_TOT_0) _
      100*(DC_VAL_2/(PGDP_2*GDP_2)-DC_VAL_0/(PGDP_0*GDP_0)) -100*(DP_G_VAL_2-DP_G_VAL_0) 100*(DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_0/(PGDP_0*GDP_0)) (FISC_2-FISC_0) 100*(r_2-r_0)
      show  a_graph_macro_2L

      '-----------***Graph Production***------------------'
       %series = ""
      For %sec {%list_sec}
        if @elem(Y_{%sec},%baseyear) <> 0 then
          %series  = %series + " 100*(Y_"+%sec+%_x+"/Y_"+%sec+"_0-1)"
        endif
      next


      graph a_graph_production2.line(m)  100*(Y_2/Y_0-1) {%series}
      show a_graph_production2

     '-----------***Graph EMS***------------------'
       %series = ""
      For %sec {%list_sec}
        if @elem(EMS_SEC_{%sec},%baseyear) <> 0 then
          %series  = %series + " 100*(EMS_SEC_"+%sec+%_x+"/EMS_SEC_"+%sec+"_0-1)"
        endif
      next

      graph a_graph_EMSsec2.line(m)  {%series}
      show a_graph_EMSsec2

     graph a_graph_ems_2V.line(m) 100*EMS_TOT_2/@elem(EMS_TOT_2,%baseyear) 100*EMS_TOT_0/@elem(EMS_TOT_0,%baseyear) 100*EMS_SEC_2/@elem(EMS_SEC_2,%baseyear) 100*EMS_SEC_0/@elem(EMS_SEC_0,%baseyear) _
                                  100*EMS_DC_2/@elem(EMS_DC_2,%baseyear) 100*EMS_DC_0/@elem(EMS_DC_0,%baseyear) 100*EMS_HH_2/@elem(EMS_HH_2,%baseyear) 100*EMS_HH_0/@elem(EMS_HH_0,%baseyear)
      show  a_graph_ems_2V

      '-----------***Graph MTEP***------------------'

      graph a_graph_Q_Mtep2.line(m)  Q_Mtep_ep_2 Q_Mtep_ep_0 Q_Mtep_ef_2 Q_Mtep_Losses_2 Q_Mtep_Losses_0 Q_Mtep_int_2 Q_Mtep_int_0 Q_Mtep_X_2 Q_Mtep_X_0
      show a_graph_Q_Mtep2

      %series = ""
      For %sec  2301 2302 2303 2304 2305 2306 2307 2308
        if @elem(Q_Mtep_ep_{%sec},%baseyear) <> 0 then
          %series  = %series + " Q_Mtep_ep_"+%sec+%_x+" Q_Mtep_ep_"+%sec+"_0"
        endif
      next

      graph a_graph_Q_Mtep_ep_elec2.line(m)  {%series}
      show a_graph_Q_Mtep_ep_elec2

      %series = ""
      For %sec 21 2201 2202 2401 2402 2403 2404 2405 2406
        if @elem(Q_Mtep_ep_{%sec},%baseyear) <> 0 then
          %series  = %series + " Q_Mtep_ep_"+%sec+%_x+" Q_Mtep_ep_"+%sec+"_0"
        endif
      next

      graph a_graph_Q_Mtep_ep_se2.line(m)  {%series}
      show a_graph_Q_Mtep_ep_se2

       %series = ""
      For %sec {%list_sec_E}
        if @elem(Q_Mtep_ep_{%sec},%baseyear) <> 0 then
          %series  = %series + " (Q_Mtep_ep_"+%sec+%_x+" - Q_Mtep_ep_"+%sec+"_0)"
        endif
      next

      graph a_graph_Q_Mtep_ep_sedif2.line(m)  {%series}
      show a_graph_Q_Mtep_ep_sedif2

      '-----------***Graph Buildings***------------------'
      %series = ""
      For %cl {%list_buil_class}
          %series  = %series+" (BUIL_"+%cl+%_x+" - BUIL_"+%cl+"_0)"
      next

      graph a_graph_buil_2.line(m) {%series} (BUIL{%_x}-BUIL_0)
      show  a_graph_buil_2


       '-----------***Graph Auto***------------------'
      %series = ""
      For %cl {%list_ener_class}
          %series  = %series+" (AUTO_"+%cl+%_x+" - AUTO_"+%cl+"_0)"
      next

      graph a_graph_AUTO_2.line(m) {%series} (AUTO{%_x}-AUTO_0)
      show  a_graph_AUTO_2

  endif



else

      '{%modelname}.makegraph(t=pcha) graph_endogene @endog
      '{%modelname}.makegraph(t=pcha) graph_exogene @exog
      '{%modelname}.makegraph(t=pcha) graph_addfactor @addfactor

endif




endsub
' *******************************************************************************************************************************
' ***************************************************** END GRAPH  SUBROUTINE *****************************************
' *******************************************************************************************************************************






' *******************************************************************************************************************************
' ***************************************************** TABLES  SUBROUTINE *****************************************
' *******************************************************************************************************************************


' This subroutine makes tables for selected variables
subroutine tables(string %tabopt)

  if %tabopt="3MEBLOCK" then

    if !scenario = 1 and %block_all="yes" then

      %_x = "_0"    'Scenario to be plotted: "_0" for baseline "_2" for shock



      '-----------***Table Macro B***------------------'
      group tab_macro_B 100*@pchy(GDPbis{%_x}) 100*@pchy(GDPter{%_x})  100*@pchy(CH{%_x}) 100*@pchy(I{%_x})  100*@pchy(G{%_x}) _
      100*@pchy(X{%_x}) 100*@pchy(M{%_x}) 100*UNR_TOT{%_x} 100*@pchy(L{%_x}) _
      100*@pchy(W{%_x}) 100*@pchy(PCH{%_x}) 100*R{%_x} 100*DP_G_VAL{%_x} 100*(DEBT_G_VAL{%_x}/(PGDPter{%_x}*GDPter{%_x})) _
      (100*DC_VAL{%_x}/(PGDPter{%_x}*GDPter{%_x})) 100*GDP{%_x}/@elem(GDP{%_x},%baseyear) _
      100*EMS_TOT{%_x}/@elem(EMS_TOT{%_x},%baseyear) 100*EMS_S{%_x}/@elem(EMS_S{%_x},%baseyear) PCH{%_x}  _
      1000000*Ttco{%_x}/PGDP{%_x} PCH{%_x} TCO_VAL{%_x}/PGDP{%_x}/1000 _
      PCH{%_x} PCH{%_x} PCH{%_x} PCH{%_x} PCH{%_x} PCH{%_x}  _ '100*TCSE{%_x} C_03{%_x} CO_val{%_x} _
      '100*(C_03_eff{%_x}/fac_conv_auto_eff)/C_03{%_x} (BUIL_eff{%_x}+BUIL_oth{%_x})
      '100*BUIL_eff{%_x}/(BUIL_eff{%_x}+BUIL_oth{%_x}) 100*BUIL_pass{%_x}/(BUIL_pass{%_x}+BUIL_fuel{%_x})
      show tab_macro_B

      '-----------***Table C02 Emissions B***------------------'
      %series = ""
      For %sec {%list_sec}
        %series  =%series + "100*(EMS_"+%sec+"_0)/@elem(EMS_"+%sec+"_0,2006)"+" 100*(Y_"+%sec+"_0)/@elem(Y_"+%sec+"_0,2006)"
      next

      group tab_CO2_sect_B {%series}
      show tab_CO2_sect_B

      '-----------***Table Consumption B***------------------'

      For %com {%list_com}
        if @elem(CH_{%com},%baseyear) <> 0 then
          %series  =%series + "100*(CH_"+%com+"_0)/@elem(CH_"+%com+"_0,2006)"
        endif
      next

      group tab_Consumption_sect_B {%series}
      show tab_Consumption_sect_B


      '-----------***Table Investissement B***------------------'

      %series = ""
      For %sec {%list_sec}
        if @elem(IA_{%sec},%baseyear) <> 0 then
          %series  =%series + "100*(IA_"+%sec+"_0)/@elem(IA_"+%sec+"_0,2006)"
        endif
      next

      group tab_Inv_sect_B {%series}
      show tab_Inv_sect_B {%series}
      '-----------***Table Employment B***------------------'
      %series = ""
      For %sec {%list_sec}
        ' if @elem(L_SE_{%sec},%baseyear) <> 0 then
        %series  = %series + " 100*@pchy(L_"+%sec+"_0)"+" 100*@pchy(Y_"+%sec+"_0)"   '"100*@pchy(L_S_"+%sec+"_0) "+" 100*@pchy(L_SE_"+%sec+"_0)"
        'endif
      next

      group tab_emploi_sect_B {%series}
      show tab_emploi_sect_B

      '---------------------***table part ENR B***---------------------'

      group tab_part_nrj_B 100*PhiY_22_2201{%_x} 100*PhiY_22_2202{%_x} 100*PhiY_23_2301{%_x} 100*PhiY_23_2302{%_x} 100*PhiY_23_2303{%_x} _
      100*PhiY_23_2304{%_x} 100*PhiY_23_2305{%_x} 100*PhiY_23_2306{%_x} 100*PhiY_23_2307{%_x} 100*PhiY_23_2308{%_x} _
      100*PhiY_24_2401{%_x} 100*PhiY_24_2402{%_x} 100*PhiY_24_2403{%_x} 100*PhiY_24_2404{%_x} 100*PhiY_24_2405{%_x} 100*PhiY_24_2406{%_x}
      'show tab_part_nrj_B


      '-----------***Table Energy efficiency B***------------------'

      %series = ""
      For %sec {%list_sec}
        %series  =%series + " 100*(EFER_"+%sec+"_0)"+"(Y_"+%sec+"_0)"
      next

      group tab_EFER_sect_B {%series}
      'show tab_EFER_sect_B

      ' '-----------***Table Unemployment B***------------------'

      ' For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
      '   %series  = %series + " 100*@pchy(UN_"+%coh+%_x+")"
      ' next

      ' group tab_Unemployment_cat_B {%series}
      'show tab_Unemployment_cat_B



      '-----------***Table Mtep B***------------------'
      %series = ""
      For %ene  21 22 2201 2202 23 2301 2302 2303 2304 2305 2306 2307 2308 24 2401 2402 2403 2404 2405 2406
        if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then
          %series  =%series + "Q_Mtep_"+%ene+"_0 "
        endif
      next
      group tab_Mtep_B {%series}
      'show tab_Mtep_B

      '-----------***Table CL PVA  B***------------------'
      %series = ""
      For %sec 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
        if @elem(PVA_{%sec},%baseyear) > 0 then
          %series  = %series + " 100*@pchy(CL_S_"+%sec+"_0)"+" 100*@pchy(PVA_"+%sec+"_0)"+" 100*@pchy(TMD_"+%sec+"_0)"   '"100*@pchy(L_S_"+%sec+"_0) "+" 100*@pchy(L_SE_"+%sec+"_0)"
        else
          %series  = %series + " 100*@pchy(CL_S_"+%sec+"_0)"+" 0"
        endif
      next

      group tab_cost_sect_B {%series}
      'show tab_cost_sect_B

      group tab_Income_B 100*@pchy(DISPINC_AI_VAL{%_x}/PCH{%_x}/WAPop_TOT) 100*@pchy(W{%_x}/PVA{%_x})

      group a_tab_baseline tab_macro_B  tab_CO2_sect_B tab_emploi_sect_B  tab_unemployment_cat_B tab_Consumption_sect_B tab_Mtep_B tab_part_nrj_B tab_EFER_sect_B tab_Inv_sect_B tab_Income_B tab_cost_sect_B'tab_cost_sect_B 'tab_CO2_DEC_sect_B
      show a_tab_baseline

    endif

    if !scenario = 2 and %block_all="yes" then
      %_x = "_2"    'Scenario to be plotted: "_0" for baseline "_2" for shock

      '-----------***Table Macro S***------------------'

      group tab_macro_S  100*(GDPbis{%_x}/GDPbis_0-1) 100*(GDPter{%_x}/GDPter_0-1) 100*(CH{%_x}/CH_0-1) 100*(I{%_x}/I_0-1)  100*(G{%_x}/G_0-1) _
      100*(X{%_x}/X_0-1) 100*(M{%_x}/M_0-1) 100*(UNR_TOT{%_x}-UNR_TOT_0) 100*(L{%_x}/L_0-1) _
      100*(W{%_x}/W_0-1) 100*(@pchy(PCH{%_x})-@pchy(PCH_0)) 100*(R{%_x}-R_0) 100*(DP_G_VAL{%_x}-DP_G_VAL_0) 100*(DEBT_G_VAL{%_x}/(PGDPter{%_x}*GDPter{%_x})-DEBT_G_VAL_0/(PGDPter_0*GDPter_0)) _
      100*(DC_VAL{%_x}/(PGDPter{%_x}*GDPter{%_x})- DC_VAL_0/(PGDPter_0*GDPter_0)) 100*GDPbis{%_x}/@elem(GDPbis{%_x},%baseyear)  _
      100*EMS_TOT{%_x}/@elem(EMS_TOT{%_x},%baseyear) 100*EMS_S{%_x}/@elem(EMS_S{%_x},%baseyear) PCH{%_x}  _
      1000000*Ttco{%_x}/PGDP{%_x} PCH{%_x} TCO_VAL{%_x}/PGDP{%_x}/1000 _
      PCH{%_x} PCH{%_x} PCH{%_x} PCH{%_x} PCH{%_x} PCH{%_x}  _ '100*TCSE{%_x} C_03{%_x} CO_val{%_x} _
      '100*(C_03_eff{%_x}/fac_conv_auto_eff)/C_03{%_x} (BUIL_eff{%_x}+BUIL_oth{%_x})
      '100*BUIL_eff{%_x}/(BUIL_eff{%_x}+BUIL_oth{%_x}) 100*BUIL_pass{%_x}/(BUIL_pass{%_x}+BUIL_fuel{%_x})

      'show tab_macro_S


      '-----------***Table C02 Emissions S***------------------'
      %series = ""
      For %sec {%list_sec}
        %series  =%series + "100*(EMS_"+%sec+"_2)/@elem(EMS_"+%sec+"_2,2006)"+" 100*(Y_"+%sec+"_2)/@elem(Y_"+%sec+"_2,2006)"
      next

      group tab_CO2_sect_S {%series}
      'show tab_CO2_sect_S

      '-----------***Table C02 Emissions Ecart Relatif***------------------'
      %series = ""
      For %sec {%list_sec}
        %series  =%series + " 100*(EMS_"+%sec+"_2/EMS_"+%sec+"_0-1)" _
        + " 100*(Y_"+%sec+"_2/@elem(Y_"+%sec+"_2,2006)-Y_"+%sec+"_0/@elem(Y_"+%sec+"_0,2006))"


      next

      group tab_CO2_sect_ER {%series}
      'show tab_CO2_sect_ER

      '-----------***Table Employment S***------------------'
      %series = ""
      For %sec {%list_sec}
        ' if @elem(L_SE_{%sec},%baseyear) <> 0 then
        %series  = %series + "100*(L_"+%sec+"_2/L_"+%sec+"_0-1)"+" 100*(Y_"+%sec+"_2/Y_"+%sec+"_0-1)"      '"100*@pchy(L_S_"+%sec+"{%_x})"+" 100*@pchy(L_SE_"+%sec+"{%_x})"+"

        ' endif
      next

      group tab_emploi_sect_S {%series}
      'show tab_emploi_sect_S

      '-----------***Table Consumption S***------------------'
      '"+" 100*(EFER_"+%sec+"{%_x})/@elem(EFER_"+%sec+"%_x},2006)"

      %series = ""
      For %com {%list_com}
        if @elem(CH_{%com},%baseyear) <> 0 then
          %series  =%series + "100*((CH_"+%com+"_2)/(CH_"+%com+"_0)-1) "
        endif
      next

      group tab_Consumption_sect_S {%series}
      'show tab_Consumption_sect_S

      '-----------***Table Investissement  S***------------------'
      '"+" 100*(EFER_"+%sec+"{%_x})/@elem(EFER_"+%sec+"%_x},2006)"

      %series = ""
      For %sec {%list_sec}

        if @elem(IA_{%sec},%baseyear) <> 0 then
          %series  =%series + "100*((IA_"+%sec+"_2)/(IA_"+%sec+"_0)-1) "
        endif
      next

      group tab_Inv_sect_S {%series}
      'show tab_Consumption_sect_S

      '---------------------***table part ENR S***---------------------'

      group tab_part_nrj_S 100*PhiY_22_2201{%_x} 100*PhiY_22_2202{%_x} 100*PhiY_23_2301{%_x} 100*PhiY_23_2302{%_x} 100*PhiY_23_2303{%_x} _
      100*PhiY_23_2304{%_x} 100*PhiY_23_2305{%_x} 100*PhiY_23_2306{%_x} 100*PhiY_23_2307{%_x} 100*PhiY_23_2308{%_x} _
      100*PhiY_24_2401{%_x} 100*PhiY_24_2402{%_x} 100*PhiY_24_2403{%_x} 100*PhiY_24_2404{%_x} 100*PhiY_24_2405{%_x} 100*PhiY_24_2406{%_x}

      'show tab_part_nrj_S


      '-----------***Table Energy efficiency S***------------------'
      '"+" 100*(EFER_"+%sec+"{%_x})/@elem(EFER_"+%sec+"%_x},2006)"

      %series = ""
      For %sec {%list_sec}
        %series  =%series + " 100*((EFER_"+%sec+"_2)/(EFER_"+%sec+"_0)-1) "+"100*((Y_"+%sec+"_2)/(Y_"+%sec+"_0)-1) "
      next

      group tab_EFER_sect_S {%series}
      'show tab_EFER_sect_S

      ' ' '-----------***Table Unemployment S***------------------'

      ' ' For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
      ' '   %series  = %series + " 100*@pchy(UN_"+%coh+%_x+")"
      ' ' next

      ' group tab_Unemployment_cat_S {%series}
      'show tab_Unemployment_cat_S

      '-----------***Table Mtep S***------------------'
      %series = ""
      For %ene  21 22 2201 2202 23 2301 2302 2303 2304 2305 2306 2307 2308 24 2401 2402 2403 2404 2405 2406

        if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then
          %series  =%series + "100*((Q_Mtep_"+%ene+"_2)/(Q_Mtep_"+%ene+"_0)-1) "
        endif
      next


      group tab_Mtep_S {%series}
      'show tab_Mtep_S

      '-----------***Table CL PVA  S***------------------'
      %series = ""
      For %sec 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
        if @elem(PVA_{%sec},%baseyear) > 0 then
          %series  = %series + "100*(CL_S_"+%sec+"_2/CL_S_"+%sec+"_0-1)"+" 100*(PVA_"+%sec+"_2/PVA_"+%sec+"_0-1)" +" 100*(TMD_"+%sec+"_2-TMD_"+%sec+"_0)"    '"100*@pchy(L_S_"+%sec+"{%_x})"+" 100*@pchy(L_SE_"+%sec+"{%_x})"+"

        else

          %series  = %series + "100*(CL_S_"+%sec+"_2/CL_S_"+%sec+"_0-1)"+" 0"
        endif
      next

      group tab_cost_sect_S {%series}
      'show tab_cost_sect_S


      group tab_Income_S 100*((DISPINC_AI_VAL{%_x}/PCH{%_x}/WAPop_TOT)/(DISPINC_AI_VAL_0/PCH_0/WAPop_TOT)-1) 100*((W{%_x}/PVA{%_x})/(W_0/PVA_0)-1)

      group a_tab_shock tab_macro_S  tab_CO2_sect_S tab_CO2_sect_ER tab_emploi_sect_S tab_unemployment_cat_S tab_Consumption_sect_S tab_Mtep_S tab_part_nrj_S tab_EFER_sect_S tab_Inv_sect_S tab_Income_S  tab_cost_sect_S 'tab_CO2_DEC_sect_S  '
      show a_tab_shock

    endif
  endif




  if %tabopt="VAR_MESANGE" then
    if !scenario = 2 and %block_all="yes" then
      %_x = "_2"    'Scenario to be plotted: "_0" for baseline "_2" for shock

      '-----------***Table Comparaison chocs mesange***------------------'


      group a_tab_macro_2  100*(GDP_2/GDP_0-1) 100*((VA_2-VA_20_2)/(VA_0-VA_20_0)-1) 100*(CH_2/CH_0-1) 100*(IA_2/IA_0-1) _
      100*(IA_19_2/IA_19_0-1) 100*(X_2/X_0-1) 100*(M_2/M_0-1) 100*(DISPINC_VAL_2/PCH_2/(DISPINC_VAL_0/PCH_0)-1) _
      100*(TS_2-TS_0) 100*(PCH_2/PCH_0-1) 100*(PYQ_2/PYQ_0-1) 100*(PX_2/PX_0-1) 100*(PM_2/PM_0-1) _
      100*((W_2/PCH_2)/(W_0/PCH_0)-1) 100*((CL_2/PY_2)/(CL_0/PY_0)-1) L_2-L_0 100*(UNR_TOT_2-UNR_TOT_0) _
      100*(DC_VAL_2/(PGDP_2*GDP_2)-DC_VAL_0/(PGDP_0*GDP_0)) -100*(DP_SP_G_VAL_2-DP_SP_G_VAL_0) 100*(DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_0/(PGDP_0*GDP_0))
      show a_tab_macro_2



    endif
  endif

endsub
'-----------***Table Macro B***------------------'
