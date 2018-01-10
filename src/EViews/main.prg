' IMPORTANT WARNING!! You may need to RUN E-views as administrator.
'run(1,c,q) main ' Run a program. c : run program file without display the program file window. v / q : verbose / quiet; ver4 / ver5 : Execute program in previous version script.


' ***************
' Configuration

include .\configuration.prg


' **********
' Includes suroutines

include .\create_lists

' Load data
include .\load_data
include .\load_calibration

' Running the model
include .\tracker.prg
include .\solve
include .\run

' Utility procedures
include .\results_outputs

' Addin: External compiler
include .\..\addin\model_addin.prg
include .\..\addin\export.prg


' ***********
' Model run

For %DC {%calibrations}

    ' Relative paths
    %data_calibration = ".\..\..\data\calibrations\"+%DC+".xls"

    call run(%data_calibration,%data_shocks)

Next


