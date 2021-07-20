' Ex SANS creation d'une nouvelle série
%baseyear = "2015"
%list_ener_class = "CA CB CC CD CE CF CG"
%list_sec_ELE =  "senu seoi sega seco sewi seso sehy sech seot"
%list_com_E = "ccoa ccoi cfut cfuh cgas cele chea cbio cote"
%list_com = "cagr cfor cfoo cveh cgla cpap cche cpla cmet cigo ccgo ccon crai croa cwat cair cpri cpub cmin ccoa ccoi cfut cfuh cgas cele chea cbio cote"
%list_GHG = "CO2 CH4 N2O SF6 HFC PFC"

' Send results to Excel 
  
%primary = " YG_toe_2 YG_toe_0 "    
    
    for %ce {%list_com_E}
      %primary = %primary + " YG_toe_"+%ce+"_2" + " YG_toe_"+%ce+"_0"
    next
    
    for %sele {%list_sec_ELE}
      %primary = %primary + " YG_toe_cele_"+%sele+"_2" + " YG_toe_cele_"+%sele+"_0"
    next

%primary = %primary + " YG_toe_cfut_soil_2 YG_toe_cfut_soil_0 YG_toe_cfut_sbfu_2 YG_toe_cfut_sbfu_0 YG_toe_cgas_sgas_2 YG_toe_cgas_sgas_0 YG_toe_cgas_sbga_2 YG_toe_cgas_sbga_0"
group Primary {%primary} 
show Primary


