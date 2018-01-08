' ***************
' Configuration

' Data calibrations to be used in the model run - file names are space-separated and found inside data\calibration
' Example: to run the NEW, NR_AS_G and NR_AS_G_WS calibrations, use "NEW NR_AS_G NR_AS_G_WS"
%calibrations = "FRA_AME"

' Shocks to run, filenames are space-separated and found inside data\shocks
' Example: to run the VATMES, TCO2 and CSEMES shocks, use "VATMES TCO2 CSEMES"
%shocks = "SCEN_ENR_100_2" 'SCEN_ADEMEhaut_test1 SCEN_ADEMEhaut_test2 SCEN_ADEMEhaut_test3 SCEN_ADEMEhaut_test4 SCEN_ADEMEhaut_test5 SCEN_ADEMEhaut_test6 SCEN_ADEMEhaut_test7 SCEN_ADEMEhaut_test8"

' Set the begining of the sample
%firstdate = "2004"
' Input the base year used for the calibration
%baseyear = "2006"
' Set the end of the sample
%lastdate = "2050"
' Set the end of the graph sample
%lastdate_graph = "2050"

' List of model extension to be run (master, hybrid, IO, etc.)
%ModelVersion = "master"

' Set "realist" for simulating a realistic reference scenario; something else for a stationary  reference scenario
%ref = "realist"


' Set "yes" for running shock scenario
%run_shock = "basic"


' ********************
' Additional options

%modelname = "a_3ME"
' Set frequency ("a" : annual; "q" quarterly)
%freq = "a"
' Set "new" for loading the data and the specification of the model; something else for loading an existing workfile
%load = "new"
' Set "u0, u1,... " for user options; "d" diagnostic option; something else for default option
%solveopt = "u0"
' Set "g0, g1,... " for user options; "" for no graph   3MEBLOCK
%graphopt = "ADEME"
' Set "t0, t1,... " for user options; "" for no table  VAR_MESANGE
'VAR_MESANGE"
%tabopt = ""
' Set the threshold under which the value is rounded to zero.
!round0 = 1.0E-10
