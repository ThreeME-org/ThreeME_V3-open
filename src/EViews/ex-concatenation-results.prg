%list_sec = "sAZ sDE sC1 sC2 sC3 sC4 sC5 sFZ sGZ sHZ sIZ sJZ sKZ sLZ sMN sOQ sRU"    

%report_sect = "100*(Y_2/Y_0-1) 100*(F_L_2/F_L_0-1) (F_L_2-F_L_0)  100*(EMS_CI_CO2_2/EMS_CI_CO2_0-1)" 
    
for %s {%list_sec}
   %report_sect = %report_sect + " 100*(Y_"+%s+"_2/Y_"+%s+"_0-1)"  
next

for %s {%list_sec}
   %report_sect = %report_sect + " 100*(F_L_"+%s+"_2/F_L_"+%s+"_0-1)"  
next

for %s {%list_sec}
   %report_sect = %report_sect + " (F_L_"+%s+"_2-F_L_"+%s+"_0)"  
next

for %s {%list_sec}
   %report_sect = %report_sect + " 100*(EMS_CI_CO2_"+%s+"_2/EMS_CI_CO2_"+%s+"_0-1)"  
next


group Sectors {%report_sect}

show Sectors
