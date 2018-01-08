' IMPORTANT WARNING!! You may need to RUN E-views as administrator.
'run(1,c,q) main ' Run a program. c : run program file without display the program file window. v / q : verbose / quiet; ver4 / ver5 : Execute program in previous version script.


' ***************
' Configuration

include .\configuration.prg


' **********
' Includes

' Utility procedures
include .\subroutines
include .\results_outputs

' Addin: External compiler
include .\..\addin\model_addin.prg
include .\..\addin\export.prg

' Load data
include .\load_calibration
include .\load_data_shocks
include .\load_data_hybrid
include .\load_data_realist
include .\standard_shocks

' Running the model
include .\tracker.prg
include .\solve
include .\run


' ***********
' Model run

For %DC {%calibrations}

    ' Relative paths
    %data_calibration = ".\..\..\data\calibrations\SAM_"+%DC+".xls"

    call run(%data_calibration,%data_shocks)

Next


