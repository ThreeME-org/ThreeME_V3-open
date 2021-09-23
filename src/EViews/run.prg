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

subroutine run(string %load, string %warnings)

'************************************************
'*********** DEFINE THE MODEL *******************
'************************************************
%wfname = ".\..\..\results\"+%modelname
' Call lists
call R_lists
call EViews_lists

if %load="workfile"  then
' ## Load the workfile of an existing model
  if %warnings = "warnings"  then
  shell(h) rundll32 user32.dll,MessageBeep 
  %name = %modelname
  %namePrompt = "Enter file name"
  !answer = @uidialog("Caption", "Open an exiting workfile", "Edit", %name, %namePrompt, 64)
  %wfname = ".\..\..\results\"+%name
      if !answer = 0 then
        wfopen {%wfname}    ' Load workfile if already created (the workfile should be in ThreeME_V3\results\)
      else
        stop
      endif

  else

  wfopen {%wfname}    ' Load workfile if already created (the workfile should be in ThreeME_V3\results\)
  endif
else
' ## Create a new model 
 
  ' Create the Workfile
  wfcreate(wf=%wfname, page=%modelname) {%freq} {%firstdate} {%lastdate}
  'Give a name to the Model
  model {%modelname}
  
  ' Load calibration data from the Excel file (if needed)
  call load_calibration

  ' Export all variables to a csv file (this file is needed to execute the external compiler)
  call export_all_to_csv

  ' Create the series using the dependencies (add-ins "series")
  statusline "Compiling the calibration of the model's variables... Please wait it may take a few minutes..."

  call load_series("..\model\lists parameters R_Calibration_FRA round0 Prices_data SU_data Special_data Other_data Exception_taxes_prices_data Exception_NestedCES_data Exception_France-FNTP_data")
  '' Exception_housing_data Exception_transport_data Exception_France-ADEME_data
  ' Exception_ConsumerNested_data  Exception_Other_data

  ' CREATE SERIES FOR THE HOUSING AND TRANSPORT BLOCKS (from the "scr/data" folder)

  ' Create the "round0" series for the housing block (initialization)
  ' call load_series("Exception_housing_data")

  ' Export all variables to a csv file (used by the external compiler)
  'call export_all_to_csv

  ' Create the series with if conditionalities (Housing block) 
  ' call load_series("Exception_housing_data_if")
    
  ' Create the series for the Housing block 
  ' call load_series("Exception_housing_data")

  ' Create the series using the dependencies (add-ins "series")
  ' call load_series("Exception_transport_data")

  ' Export all variables to a csv file (used by the external compiler)
  call export_all_to_csv

  ' Load the model specification from the model/ folder
  statusline "Compiling the equations of the model... Please wait it may take a few minuts..."
  call load_model("blocks")

' Save the workfile before running simulation
if %load = "new" then
    if %warnings = "warnings"  then
      shell(h) rundll32 user32.dll,MessageBeep 
      %name = %modelname
      %namePrompt = "Enter file name"
      !answer = @uidialog("Caption", "Save workfile before simulation?", "Edit", %name, %namePrompt, 64)
      %wfname = ".\..\..\results\"+%name
      if !answer = 0 then
         wfsave {%wfname}
      endif
    endif
endif
  ' Clean up results folder
  %cmd = @left(@linepath, 2) + " & cd " + @addquotes(@linepath) + " & del /Q ..\..\results\*.txt"
  shell(h) {%cmd}
endif

' Checking that equations are balanced at baseyear (add factors = 0)
  if %load = "new" and %warnings = "warnings" then 
    call checkaddfactor(%modelname,1e-5)
  endif



'************************************************
'*********** SOLVE SCENARIOS ********************
'************************************************
' ***************************************
' Call here the subroutine you want to use to solve the shock
call run_scenario("baseline-steady")

'call run_scenario("protechno")

'call run_scenario("protechno_2")

call run_scenario("protechno_3")

'call run_scenario("carbontax_s1")

' ****************************************
'call run_standard("EXR10 RSSC1 VAT1 INCT1 WD1 FF10 CT1", %iso3, 1) ' Option: 1 for result in excel template; 0 only scenario run

'call run_standard("EXR10 EXPG1 RSSC1 VAT1 INCT1 WD1 FF10 CT1", %iso3, 1) ' Option: 1 for result in excel template; 0 only scenario run


' ***************************************
' Call (eventually) here the subroutine you want to use to analyse the results

' Creation of SU table on value and in volume
'call create_sut("2015 2020 2030 2050", "0 2")

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
' Saving workfile with simulation
if %warnings = "warnings"  then
  shell(h) rundll32 user32.dll,MessageBeep 
  string name = %modelname+"_res"
  string namePrompt = "Enter file name"
  scalar answer = @uidialog("Caption", "Save workfile result", "Edit", name, namePrompt, 64)
  %wfname = ".\..\..\results\"+name
  if answer = 0 then
       wfsave {%wfname}
  endif
endif
endsub


