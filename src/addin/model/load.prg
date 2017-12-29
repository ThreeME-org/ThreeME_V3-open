' The core subroutine of the addin
' Sends an equation to the compiler,
' then retrieve the compiled output and adds it to the model

' The equation to be compiled is passed in as arguments
' It must first be written to file in.txt
%compilerInPath = @linepath + "in.txt"
text blockIn
do blockIn.clear
for %block {%args}
  %tmpLine = "include ..\..\model\" + %block
  do blockIn.append %tmpLine
next
do blockIn.save %compilerInPath

' Run the compiler
%compilerpath = @linepath + "compiler.exe"
%compilercmd = @left(@linepath, 2) + " & cd " + @addquotes(@linepath) + " & " + @addquotes(%compilerpath)
shell(h, t=500000) {%compilercmd}

' Read compiled equations from file out.txt
%compilerOutPath = @linepath + "out.txt.prg"
text compiledEquations
compiledEquations.clear
compiledEquations.append(file) %compilerOutPath

' Check if an error was returned
if compiledEquations.@line(1) == "Error" then
  %error_msg = "Error while compiling " + @addquotes(%args) + ": "
  for !jEq = 2 to compiledEquations.@linecount
    %error_msg = %error_msg + compiledEquations.@line(!jEq)
  next
  @uiprompt(%error_msg)
  stop
' Otherwise add compiled equations to the model
else
  for !jEq = 1 to compiledEquations.@linecount
    %tmpeq = compiledEquations.@line(!jEq)
    if @len(%tmpeq) > 0 then
      _this.append {%tmpeq}
    endif
  next
endif
