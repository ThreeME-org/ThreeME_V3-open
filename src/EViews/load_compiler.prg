' These are the core subroutine to run the compiler from Eviews
' Sends the equation to the compiler,
' then retrieve the compiled output and adds it to the model (calibration)

' The equation to be compiled is passed in as arguments
' It must first be written to file in.txt

' ****************************************************************** '
' Subroutine that run the compiler for the calibration of the series
subroutine load_series(string %args)

' Create starting _tmp_all_vars.csv if it does not exist
%pathcsv= @linepath + "..\compiler\_tmp_all_vars.csv"
if @fileexist(%pathcsv)=0 then
  smpl {%baseyear} {%baseyear}
  delete(noerr) all_vars_table
  series start_tmp_all_vars = 0
  group all_vars start_tmp_all_vars
  freeze(all_vars_table) all_vars
  all_vars_table.save(t=csv) %pathcsv
  smpl @all
endif

' Relative path of the compiler folder
%path= @linepath + "..\compiler\"

' Write the input of the data folder in a in.txt file
%compilerInPath = %path + "in.txt"
text blockIn
do blockIn.clear
for %block {%args}
  %tmpLine = "include ..\data\" + %block
  do blockIn.append %tmpLine
next
do blockIn.save %compilerInPath

' Run the compiler
if %compiler = "exec" then
  ' Use of the compiler.exe
  %compilerpath = "compiler_2.0.exe" 
else
  ' Use of the compiler.py (developing)
  %compilerpath = "python.exe compiler.py" 
endif

%compilercmd = @left(@linepath, 2) + " & cd " + %path + " & "+ %compilerpath
shell(h, t=500000) {%compilercmd} dependencies

' Save file out.txt in Eviews folder
%compilerOutPath = %path + "out.txt.prg"
%cmd = "copy " + %compilerOutPath + " " + "series_eviews.prg"
shell(h) {%cmd}

' Compile the series in Eviews
exec(c, q) .\series_eviews.prg

endsub

' ****************************************************************** '
' Subroutine that run the compiler for loading the equations of the model
subroutine load_model(string %args)

' Create starting _tmp_all_vars.csv if it does not exist
%pathcsv= @linepath + "..\compiler\_tmp_all_vars.csv"
if @fileexist(%pathcsv)=0 then
  smpl {%baseyear} {%baseyear}
  delete(noerr) all_vars_table
  series start_tmp_all_vars = 0
  group all_vars start_tmp_all_vars
  freeze(all_vars_table) all_vars
  all_vars_table.save(t=csv) %pathcsv
  smpl @all
endif

' Relative path of the compiler folder
%path= @linepath + "..\compiler\"

' Write the input of the data folder in a in.txt file
%compilerInPath = %path + "in.txt"
text blockIn
do blockIn.clear
for %block {%args}
  %tmpLine = "include ..\model\" + %block
  do blockIn.append %tmpLine
next
do blockIn.save %compilerInPath

' Run the compiler
if %compiler = "exec" then
  'Use of the compiler.exe
  %compilerpath = "compiler_2.0.exe" 
else
  'Use of the compiler.py (developing)
  %compilerpath = "python.exe compiler.py" 

endif

%compilercmd = @left(@linepath, 2) + " & cd " + %path + " & "+ %compilerpath
shell(h, t=500000) {%compilercmd}

' ---------------------------------------------------------'
' Section  between ' --------------- ' will be be deleted when the compiler write direcly each equation of the model by starting with "model_eviews.append"

' Read compiled equations from file out.txt into a text Eviews object
%compilerOutPath = %path + "out.txt.prg"
text compiledEquations
compiledEquations.clear
compiledEquations.append(file) %compilerOutPath

text model_eviews
model_eviews.clear

' Compile the model_eviews.prg
for !jEq = 1 to compiledEquations.@linecount
    %tmpeq = %modelname + ".append " + compiledEquations.@line(!jEq)
    if @len(%tmpeq) > 0 then
      model_eviews.append {%tmpeq}
    endif
next

' Save the model_eviews.prg and delete model_eviews text object 
model_eviews.save(t=txt) model_eviews.prg
delete model_eviews

' ---------------------------------------------------------'

' Compile the model object in Eviews
exec(c, q) .\model_eviews.prg

' Report model error
freeze(modelerror) {%modelname}.spec
if modelerror.@line(1) == "Error" then
  show modelerror
  stop
endif

endsub

' ****************************************************************** '
' Export all variables to a csv file
' This is used to communicate with the compiler
subroutine export_all_to_csv

  smpl {%baseyear} {%baseyear}
  delete(noerr) all_vars_table
  group all_vars *
  freeze(all_vars_table) all_vars
  %pathcsv= @linepath + "..\compiler\_tmp_all_vars.csv"
  all_vars_table.save(t=csv) %pathcsv
  
  smpl @all
endsub

