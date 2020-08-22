' ============================================================================
' This file defines the configuration options
'
' ============================================================================

' Set the begining of the sample
%firstdate = "2012"
' Input the base year used for the calibration
%baseyear = "2015"

' Set the end of the sample
%lastdate = "2050"
' Data calibrations to be used in the model run - file names are space-separated and found inside data\calibration
' Example: to run the NEW, NR_AS_G and NR_AS_G_WS calibrations, use "NEW NR_AS_G NR_AS_G_WS"
%calibrations = "DATA"
' Shocks to run, filenames are space-separated and found inside data\shocks
' Example: to run the VATMES, TCO2 and CSEMES shocks, use "VATMES TCO2 CSEMES"
%shocks = "neuro"
' Set "realist" for simulating a realistic reference scenario; something else for a stationary  reference scenario
%ref = ""

%sensitivity = " "

' ********************
' Additional options

' Define model name
%modelname = "a_3ME"
' Set "u0, u1,... " for user options; "d" diagnostic option; something else for default option
%solveopt = "u0"
' Set the threshold under which the value is rounded to zero.
!round0 = 1.0E-10
' Set frequency ("a" : annual; "q" quarterly)
%freq = "a"