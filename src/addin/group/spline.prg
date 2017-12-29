' Spline - Group addin
'
' Apply the spline addin on each of the series contained in this group

for !i = 1 to _this.@count
  %series = _this.@seriesname(!i)
  if @hasoption("y") then
    {%series}.spline(y) {%args}
  else
    {%series}.spline {%args}
  endif
next