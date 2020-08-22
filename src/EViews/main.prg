' IMPORTANT WARNING!! You may need to RUN E-views as administrator.
'run(1,c,q) main ' Run a program. c : run program file without display the program file window. v / q : verbose / quiet; ver4 / ver5 : Execute program in previous version script.
' ***************
' Configuration
include .\configuration.prg
' **********
' Includes suroutines
include .\R_lists
include .\EViews_lists
' Load data
include .\load_data
include .\load_calibration

' Running the model
include .\setThreeMe.prg
include .\tracker.prg
include .\solve
include .\sensitivity
include .\run
include .\run_extra
' Utility procedures
include .\results_outputs

' Addin: External compiler
include .\..\addin\model_addin.prg
include .\..\addin\export.prg
' ***********
' Compile and run the model (option1 = "new") or run an existing model (option1 = "workfile"). 
' Can run with warning messages (Option2 = "warnings") or with no warning messages  (Option2 = "nowarnings").
call run("new", "warnings")

smpl %baseyear @last


