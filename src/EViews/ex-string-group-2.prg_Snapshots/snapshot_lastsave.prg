' Ex SANS creation d'une nouvelle série
%baseyear = "2015"
%list_ener_class = "CA CB CC CD CE CF CG"
' Send results to Excel 
  
    %hybrid_2 = "AUTO_2  AUTO_cfut_2  AUTO_cele_2 AUTO_cgas_2"
    
   for %ecl {%list_ener_class}
        %hybrid_2 = %hybrid_2 + " AUTO_"+%ecl+"_cfut_2"
   next
    

   for %ecl {%list_ener_class}
          %hybrid_0 = %hybrid_0 + " AUTO_"+%ecl+"_cfut_0"
   next 
         
%hybrid = %hybrid_2 + %hybrid_0 
    
    group Hybrid {%hybrid} 

    show Hybrid


