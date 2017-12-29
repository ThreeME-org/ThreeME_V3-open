wfcreate y 2006 2050

series test = @rlogistic
%test = "test"

%args = "2010 0.05 2030 0.01 2050 0.08"

%bckp_smpl = @pagesmpl

if @mod(@wcount(%args), 2) = 1 then
	@uiprompt("Need an even number of arguments")
else
      !nargs = @wcount(%args) / 2

      %years = @getnextname("years")
      %values = @getnextname("values")

      vector(!nargs) {%years}
      vector(!nargs) {%values}

      for !i = 1 to !nargs
	    {%years}(!i) = @word(%args, !i * 2 - 1)
	    {%values}(!i) = @word(%args, !i * 2)
      next

  %sample = @getnextname("sample")
  !start_year = {%years}(1)
  !last_year = {%years}(!nargs)
  sample {%sample} !start_year !last_year

smpl {%sample}
%growth_series = @getnextname("growth_series")
%tmp_series = @getnextname("tmp_series")
series {%tmp_series} = NA

for !i = 1 to !nargs
	!year = {%years}(!i)
	smpl !year !year
	{%tmp_series} = {%values}(!i)
next

smpl {%sample}
{%tmp_series}.ipolate {%growth_series}
delete(noerr) {%tmp_series}

{%test} = {%test}(-1) * (1 + {%growth_series})

delete(noerr) {%growth_series}

smpl {}

endif