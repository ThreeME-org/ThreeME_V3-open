include .\create_sut

%baseyear = "2015"
%list_com = "cagr cfor cfoo cveh cgla cpap cche cpla cmet cigo ccgo ccon crai croa cwat cair cpri cpub cmin ccoa ccoi cfut cfuh cgas cele chea cbio cote"
%list_sec = "sagr sfor sfoo sveh sgla spap sche spla smet sigo scgo scon srai sroa swat sair spri spub smin soil sbfu sgas sbga senu seoi sega seco sewi seso sehy sech seot"
%list_com_MARG = "crai croa cwat cair cpri"

for %obj TAB2020S0 TAB2015S0 TAB2020S2 TAB2015S2 SUTALL
	if @isobject(%obj)=1 then
		delete %obj
	else	
	endif
next 


call create_sut("2015 2020 2030", "0 2")

