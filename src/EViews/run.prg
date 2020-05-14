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
' - loads the calibration of the variables
' - loads the specification of the model
' - run scenario(s)

subroutine run(string %load)

'************************************************
'*********** DEFINE THE MODEL *******************
'************************************************
%wfname = ".\..\..\results\"+%modelname

' Call lists
call R_lists
call EViews_lists

if %load="workfile"  then
' ## Load the workfile of an existing model 
  @uiprompt("We will load the following Workfile : ...\ThreeME_V3\results\"+%modelname+".wf1")
  wfopen {%wfname}    ' Load workfile if already created (the workfile should be in ThreeME_V3\results\)

else
' ## Create a new model 
 
  ' Create the Workfile
  wfcreate(wf=%wfname, page=%modelname) {%freq} {%firstdate} {%lastdate}

  'Give a name to the Model
  model {%modelname}
  
  ' Load calibration data from the Excel file (if needed)
  ' call load_calibration

  ' Create the series using the dependencies (add-ins "series")
  {%modelname}.series round0 Prices_data SU_data Special_data Other_data Exception_taxes_prices_data Exception_NestedCES_data Exception_Other_data Exception_Covid_data 
  '' Exception_ConsumerNested_data
  
  ' Export all variables to a csv file (used by the external compiler)
  call export_all_to_csv

  ' Load the model specification from the model/ folder
  {%modelname}.load blocks

  ' # Put add factors to all equations
  smpl @all
  {%modelname}.addassign @all
  ' # Set add factor values so that the equation has no residual when evaluated at actuals
  {%modelname}.addinit(v=n) @all
  ' # Show all add factors
  group a_addfactors *_a
  show a_addfactors

if %load = "new" then
    ' Save the workfile before running simulation
    scalar answer = @uiprompt("Would you like to save the workfile before running any simulation ? It will be saved as : ...\ThreeME_V3\results\"+%modelname+".wf1", "YN")
    if answer = 1 then
        wfsave {%wfname}
    endif
endif

  ' Clean up results folder
  %cmd = @left(@linepath, 2) + " & cd " + @addquotes(@linepath) + " & del /Q ..\..\results\*.txt"
  shell(h) {%cmd}


endif


'************************************************
'*********** SOLVE SCENARIOS ********************
'************************************************
!scenario = 0

' ***************************************
' Call here the subroutine you want to use to solve the shock
call run_scenario("baseline-steady")
 
call run_scenario("covid")

' ****************************************
' call run_standard("EXPG1", %iso3, 1)
' call run_standard("CT1 CT2 SUB1 SUB2 SUB3 EXPG1 INCT1 VAT1 FF10", %iso3, 1) ' Option: 1 for result in excel template; 0 only scenario run

'' call run_standard("RSSC1 EXR10 WD1", %iso3, 1) ' Option: 1 for result in excel template; 0 only scenario run

' ***************************************
' Call (eventually) here the subroutine you want to use to analyse the results
'' call additional_outputs("Fred")

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

endsub
