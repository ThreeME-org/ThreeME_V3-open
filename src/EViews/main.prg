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
include .\tracker.prg
include .\solve
include .\run
include .\run_extra


' Utility procedures
include .\results_outputs

' Addin: External compiler
include .\..\addin\model_addin.prg
include .\..\addin\export.prg


' ***********
' Model run

If %shocks = "euro" then

  for %iso3 FRA GBR DEU
     call run_euro(%iso3)
  next

else

  For %DC {%calibrations}

      ' Relative paths
      %data_calibration = ".\..\..\data\calibrations\"+%DC+".xls"

      call run(%data_calibration,%data_shocks)

  Next

endif

smpl 2000 @last
