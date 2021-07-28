' Ex SANS creation d'une nouvelle série
%baseyear = "2015"
%list_ener_class = "CA CB CC CD CE CF CG"
%list_sec_ELE =  "senu seoi sega seco sewi seso sehy sech seot"
%list_com_E = "ccoa ccoi cfut cfuh cgas cele chea cbio cote"
%list_com = "cagr cfor cfoo cveh cgla cpap cche cpla cmet cigo ccgo ccon crai croa cwat cair cpri cpub cmin ccoa ccoi cfut cfuh cgas cele chea cbio cote"
%list_GHG = "CO2 CH4 N2O SF6 HFC PFC"
%list_sec_AGGREG = "agrf ind trsp ser trsf ele"
%list_sec = "sagr sfor sfoo sveh sgla spap sche spla smet sigo scgo scon srai sroa swat sair spri spub smin soil sbfu sgas sbga senu seoi sega seco sewi seso sehy sech seot"
' Send results to Excel 
  
%value_added = " VA_2 VA_0 "    
    
    for %s {%list_sec}
      %value_added = %value_added + " VA_"+%s+"_2" + " VA_"+%s+"_0"
    next
    
    ' doesn't work (but works in EX-STRING-GROUP-2...)
    for %sagg {%list_sec_AGGREG}
     %value_added = %value_added + " VA_"+%sagg+"_2" + " VA_"+%sagg+"_0"
   next
group Value_Added {%value_added}
show Value_Added
