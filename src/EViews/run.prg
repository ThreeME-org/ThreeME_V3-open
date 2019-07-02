' ============================================================================
' Series of subroutines used to run the model

' Additional user defined run subroutines in the file "run_extra.prg"
' Here only the basic run subroutine!
' ============================================================================

' ============================================================================
' +++++++++++++++++++
' Subroutine "Run"
' +++++++++++++++++++
' Called from Eviews by the progam main
' Performs a complete model run:
' - loads the data calibration specified
' - loads the specification of the model
' - run scenario(s)

subroutine run(string %data_calibration, string %data_shock)

' ***********************
' Create the Workfile
%wfname = ".\..\..\results\"+%modelname+"_"+%DC
wfcreate(wf=%wfname, page=%modelname) {%freq} {%firstdate} {%lastdate}

call EViews_lists
call R_lists

' Used by setThreeMe
 call loadThreeMeLists

' ******************
' Load the model

if %load="new"  then
  'Give a name to the Model
  model {%modelname}
  ' Load calibration data from the Excel file
  call load_calibration

  ' Create the series using the dependencies (add-ins "series")
  ''''' {%modelname}.series round0 Prices_data SU_data Special_data Other_data Exception_Hybrid_data_0

  ' Export all variables to a csv file (used by the external compiler)
  call export_all_to_csv

  ''''' {%modelname}.series Exception_Hybrid_data_if

  ' Create the series using the dependencies (add-ins "series")
  ''''' {%modelname}.series Exception_Hybrid_data

  ' Create the series using the dependencies (add-ins "series")
  {%modelname}.series ..\model\lists parameters R_Calibration

  ' Export all variables to a csv file (used by the external compiler)
  call export_all_to_csv

  {%modelname}.series round0 Prices_data SU_data Special_data Other_data Exception_taxes_prices_data Exception_NestedCES_data Exception_ConsumerNested_data Exception_Other_data

  ' Export all variables to a csv file (used by the external compiler)
  call export_all_to_csv


  ' Load the model specification from the model/ folder
  {%modelname}.load blocks

  ' Put add factors to all equations
  smpl 1997 2000
  '' {%modelname}.addassign @all
  ' Set add factor values so that the equation has no residual when evaluated at actuals
  '' {%modelname}.addinit(v=n) @all
  ' Show all add factors
  '' group a_addfactors *_a
  '' show a_addfactors

  smpl @all



  ' Save the workfile for memory
  wfsave {%wfname}

else

  ' If the data are already initailized as a Workfile with the option  %load = ""
  ' Load the data
  wfopen {%wfname}    ' Load workfile if already created (the workfile should be in ThreeME_V3\results\

endif



if %sensitivity = "yes" then

  wfclose(noerr)
  wfclose(noerr)

  !iteration = 0
  for %redis_ls 0 1
    for %wage_eq 0 '1 2
      for %flex 0 1
        for %expo 0 ' 1
          wfopen {%wfname}
          series redis_ls = {%redis_ls}
          series wage_eq = {%wage_eq}
          series flex = {%flex}
          series expo = {%expo}
          call sensitivity({%redis_ls}, {%wage_eq}, {%flex}, {%expo})
          call run_scenario("baseline")
          call run_standard("CT1", %iso3, 1)
          call sensitivity_results(!iteration)
          !iteration = !iteration + 1
        next
      next
    next
  next

else

  smpl @all

  ' Clean up results folder
  %cmd = @left(@linepath, 2) + " & cd " + @addquotes(@linepath) + " & del /Q ..\..\results\*.txt"
  'shell(h) {%cmd}

  '************************************************
  '*********** SOLVE SCENARIOS ********************
  '************************************************
  !scenario = 0

  ' ***************************************
  ' Call here the subroutine you want to use to solve the shock

  'call run_baseshock ' Perform a baseline and a shock

  call run_scenario("baseline")

  'call run_standard("EXR10", 0)
  call run_standard("CT1", %iso3, 1)
  'call run_standard("EXR10 EXPG1 RSSC1 INCT1 VAT1 WD1 FF10 CT1", %iso3, 1) ' Option: 1 for result in excel template; 0 only scenario run
  ''  call run_standard("CT1", 1)

  '' call output_template("EXR10")



  ' call run_enr("PPE_ENRhaut")

  ' ***************************************
  ' Call (eventually) here the subroutine you want to use to analyse the results
  '' call additional_outputs("Fred")
  '' call additional_outputs("Fred_Hybrid")
  ' call output_template(%scenario_name)

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

endif

endsub
