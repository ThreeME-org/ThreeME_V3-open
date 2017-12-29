wfcreate u 1

delete(noerr) winver
shell(out=winver) reg query "HKLM\Software\Microsoft\Windows NT\CurrentVersion" /v "ProductName"

!is_win7 = 0

if @instr(winver(3, 1), "Windows 7") > 0 then
	!is_win7 = 1
endif

delete(noerr) addin_ini

shell(h, t=1000, out=ini_path_table) "echo %APPDATA%"

' Define location of ProgReg.ini filename based on default Eviews version.

if @vernum = 8 then
  if !is_win7 > 0 then
    %ini_path = ini_path_table(1, 1) + "\Quantitative Micro Software\EViews\ProgReg.ini"
  else
    %ini_path = ini_path_table(1, 1) + "/ihs eviews/Eviews/ProgReg.ini"
  endif
else
    %ini_path = ini_path_table(1, 1) + "\Quantitative Micro Software\EViews\ProgReg.ini"
endif

subroutine check_if_installed(text _addin_ini, string _proc_name, scalar _is_installed)
  _is_installed = 0
  for !j = 2 to _addin_ini.@linecount
    if @instr(_addin_ini.@line(!j), _proc_name) > 0 then
      _is_installed = _is_installed + 1
    endif
  next
endsub


' Initialise Addin ini file
text addin_ini

if @fileexist(%ini_path) then
  addin_ini.append(file) %ini_path
else
  addin_ini.append ""
  addin_ini.append ""
endif


subroutine install_addin(string %obj, string %proc)
  !is_installed = 0
  call check_if_installed(addin_ini, %proc, !is_installed)
  ' If the addin is not installed yet
  if !is_installed == 0 then
    addin(type=%obj, proc=%proc) .\{%obj}\{%proc}.prg
  endif
endsub

call install_addin("model", "series")
call install_addin("model", "load")
call install_addin("model", "fit")
call install_addin("series", "spline")
call install_addin("group", "spline")

wfclose
