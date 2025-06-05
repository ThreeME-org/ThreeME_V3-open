%bckp_smpl = @pagesmpl

if @hasoption("log") then
  %interpolation_type = "log"
else
  %interpolation_type = "cb"
endif

if @wcount(%args) = 1 then
  ' Do nothing if there's only one argument - no interpolation needed
else
  ' Only years are defined - the values are taken from the existing series directly
  if @hasoption("y") then
    !nargs = @wcount(%args)

    %years = @getnextname("years")
    %values = @getnextname("values")

    vector(!nargs) {%years}
    vector(!nargs) {%values}

    for !i = 1 to !nargs
	  {%years}(!i) = @word(%args, !i)
	  {%values}(!i) = @elem(_this, @word(%args, !i))
    next

  else
    if @wcount(%args) = 2 then
      %years = %0
      %values = %1
      !nargs = {%years}.@rows
    else
      if @mod(@wcount(%args), 2) > 0 then
	    @uiprompt("The number of arguments passed to interpolate must be pair.")
      endif

      !nargs = @wcount(%args) / 2

      %years = @getnextname("years")
      %values = @getnextname("values")

      vector(!nargs) {%years}
      vector(!nargs) {%values}

      for !i = 1 to !nargs
	    {%years}(!i) = @word(%args, !i * 2 - 1)
	    {%values}(!i) = @word(%args, !i * 2)
      next

    endif
  endif


  ' Define the sample over which we will interpolate the values
  %sample = @getnextname("sample")
  !start_year = {%years}(1)
  !last_year = {%years}(!nargs)
  sample {%sample} !start_year !last_year

  ' First set the series to NA over the whole sample
  smpl {%sample}
  _this = NA

  ' Set the keypoint values
  for !i = 1 to !nargs
    !year = {%years}(!i)
    smpl !year !year
    _this = {%values}(!i)
  next

  smpl {%sample}
  %tmp_series = @getnextname("tmp_series")
  if %interpolation_type = "log" then
    _this.ipolate(type=log) {%tmp_series}
  else
    _this.ipolate(type=cb) {%tmp_series}
  endif
  _this = {%tmp_series}
  delete(noerr) {%tmp_series}

  smpl {%bckp_smpl}
endif