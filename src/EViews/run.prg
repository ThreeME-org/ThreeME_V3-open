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


  '************************************************
  '*********** CREATE ThreeME MODEL ***************
  '************************************************


' *************************************************
' CREATE THE EVIEWS WORKFILE

%wfname = ".\..\..\results\"+%modelname+"_"+%DC
wfcreate(wf=%wfname, page=%modelname) {%freq} {%firstdate} {%lastdate}

call EViews_lists
call R_lists

' Used by setThreeMe
 call loadThreeMeLists

' *************************************************
' LOAD THE MODEL

if %load="new"  then
  'Give a name to the Model
  model {%modelname}
  ' Load calibration data from the Excel file
  call load_calibration

' CREATE SERIES FOR THE MASTER VERSION (from the "scr/data" folder)
  ' Create the series using the dependencies (add-ins "series")
  {%modelname}.series ..\model\lists parameters R_Calibration_FRA round0 Prices_data SU_data Special_data Other_data Exception_taxes_prices_data Exception_NestedCES_data
  ' Exception_ConsumerNested_data  Exception_Other_data

' CREATE SERIES FOR THE HOUSING AND TRANSPORT BLOCKS (from the "scr/data" folder)

    ' Create the "roun0" series for the housing block (initialization)
    {%modelname}.series Exception_housing_data_0

    ' Export all variables to a csv file (used by the external compiler)
    call export_all_to_csv

    ' Create the series with if conditionalities (Housing block) 
    {%modelname}.series Exception_housing_data_if
    
    ' Create the series for the Housing block 
    {%modelname}.series Exception_housing_data

    ' Create the series using the dependencies (add-ins "series")
    {%modelname}.series Exception_transport_data

' LOAD THE MODEL EQUATIONS

  ' Export all variables to a csv file (used by the external compiler)
  call export_all_to_csv

  ' Load the model specification from the "scr/model" folder
  {%modelname}.load blocks

  ' Put add factors to all equations
  smpl %firstdate %baseyear
  '' {%modelname}.addassign @all
  ' Set add factor values so that the equation has no residual when evaluated at actuals
  '' {%modelname}.addinit(v=n) @all
  ' Show all add factors
  '' group a_addfactors *_a
  '' show a_addfactors

  smpl @all


  ' Save the workfile for memory
  'wfsave {%wfname}

else

  ' If the data are already initailized as a Workfile with the option  %load = ""
  ' Load the data
  wfopen {%wfname}    ' Load workfile if already created (the workfile should be in ThreeME_V3\results\

endif


  '************************************************
  '*********** RUN ThreeME MODEL ******************
  '************************************************

' *************************************************
' RUN SENSITIVITY ANALYSIS (see wiki page)

if %sensitivity = "yes" then

  wfclose(noerr)
  wfclose(noerr)

  !max_iteration = 2 * 3 * 2 * 2
  !iteration = 0
  for %redis_ls 0 1
    for %wage_eq 0 1 2
      for %flex 0 1
        for %expo 0 1
          wfopen {%wfname}
          series redis_ls = {%redis_ls}
          series wage_eq = {%wage_eq}
          series flex = {%flex}
          series expo = {%expo}
          call sensitivity({%redis_ls}, {%wage_eq}, {%flex}, {%expo})
          call run_scenario("baseline")
          call run_standard("CT1", %iso3, 0)
          call sensitivity_results(!iteration, !max_iteration)
          !iteration = !iteration + 1
          wfclose(noerr)
        next
      next
    next
  next

else


' *************************************************
' RUN SENARIOS

  smpl @all

  ' Clean up results folder
  %cmd = @left(@linepath, 2) + " & cd " + @addquotes(@linepath) + " & del /Q ..\..\results\*.txt"
  shell(h) {%cmd}

  !scenario = 0

  ' ***************************************
  ' Call here the subroutine you want to use to solve the shock

  'call run_baseshock ' Perform a baseline and a shock

  call run_scenario("baseline")
 
  ' call run_standard("EXR10", %iso3, 0) ' Option: 1 for result in excel template; 0 only scenario run
  ' call run_standard("RSSC1", %iso3, 1)
  ' call run_standard("EXR10 EXPG1 RSSC1 INCT1 VAT1 WD1 FF10 CT1", %iso3, 1) ' Option: 1 for result in excel template; 0 only scenario run
  ''  call run_standard("CT1", 1)

  '' call output_template("EXR10")


  ' call run_enr("PPE_ENRhaut")

  ' ***************************************
  ' Call (eventually) here the subroutine you want to use to analyse the results
  '' call additional_outputs("Fred")
  call additional_outputs("Fred_housing")
  call additional_outputs("Fred_transport")
  
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
