' wfcreate u 1

subroutine parseClean(string %l)
  ' Special case for O
  if %l = "O in D M" then
    %l = ""
  endif

  if @left(%l, 1) = "#" then
	%l = ""
  endif

  !commentStart = @instr(%l, "#")
  if !commentStart > 0 then
	%l = @left(%l, !commentStart - 1)
  endif

  string parseLine = %l
endsub

subroutine parseList(string %l)
  if @len(%l) > 0 then
	%l = @replace(@replace(%l, ":=", "") , "%", "")
	%def = @word(%l, 1) + " = """  + @wright(%l, @wcount(%l) - 1) + """"
	string {%def}
  endif
endsub

subroutine parseIndex(string %l)
  if @len(%l) > 0 then
	%l = @replace(@replace(%l, " in ", " ") , "%", "")
	' Special case for `c'
	%list = @wright(%l, 1)
	if @word(%l, 1) = "c" then
	  %def = "c_3me = """  + {%list} + """"
	else
	  if @word(%l, 1) = "m" then
        %def = "m_3me = """  + {%list} + """"
      else
        %def = @word(%l, 1) + " = """  + {%list} + """"
      endif
	endif
	string {%def}
  endif
endsub


subroutine loadThreeMeLists
  text R_lists
  R_lists.append(file) .\..\model\R_lists.mdl

  for !j = 1 to R_lists.@linecount
	%l = @trim(R_lists.@line(!j))
	call parseClean(%l)
	%l = parseLine
	call parseList(%l)
  next

  text lists
  lists.append(file) .\..\model\lists.mdl

  for !j = 2 to lists.@linecount

	%l = @trim(lists.@line(!j))
	call parseClean(%l)
	%l = parseLine

	if @instr(%l, ":=") then
	  call parseList(%l)
	endif

	if @instr(%l, " in ") then
	  call parseIndex(%l)
	endif

  next
endsub

subroutine parseThreeMeVar(string %v)
  %v = @replace(@replace(@replace(%v, ",", " "), "[", " "), "]", " ")

  string root_3me = @word(%v, 1)

  if @wcount(%v) > 1 then
	for !j = 1 to (@wcount(%v) - 1)
	  %index = "i" + @str(!j) + "_n_3me"
	  %definition = @word(%v, !j + 1)
	  if %definition = "c" then
		%definition = "c_3me"
      else
        if %definition = "m" then
          %definition = "m_3me"
        endif
	  endif
	  %def = %index + " = " + %definition
	  string {%def}
	next
  endif

endsub

subroutine setThreeMe(string %v, scalar val)

  call parseThreeMeVar(%v)

  if @wcount(%v) = 1 then
	%def = %v + " = " + @str(val)
	series {%def}
  endif

  if @wcount(%v) = 2 then
	for %i1 {i1_n_3me}
	  %def = root_3me + "_" + %i1 + " = " + @str(val)
	  series {%def}
	next
  endif

  if @wcount(%v) = 3 then
	for %i1 {i1_n_3me}
	  for %i2 {i2_n_3me}
		%def = root_3me + "_" + %i1 + "_" + %i2 + " = " + @str(val)
		series {%def}
	  next
	next
  endif

  if @wcount(%v) = 4 then
	for %i1 {i1_n_3me}
	  for %i2 {i2_n_3me}
		for %i3 {i3_n_3me}
		  %def = root_3me + "_" + %i1 + "_" + %i2 + "_" + %i3 + " = " + @str(val)
		  series {%def}
		next
	  next
	next
  endif

  if @wcount(%v) = 5 then
	for %i1 {i1_n_3me}
	  for %i2 {i2_n_3me}
		for %i3 {i3_n_3me}
		  for %i4 {i4_n_3me}
			%def = root_3me + "_" + %i1 + "_" + %i2 + "_" + %i3 + "_" + %i4 + " = " + @str(val)
			series {%def}
		  next
		next
	  next
	next
  endif

endsub

'call loadThreeMeLists
'call setThreeMe("K[ce, cee, s]", 0.2)