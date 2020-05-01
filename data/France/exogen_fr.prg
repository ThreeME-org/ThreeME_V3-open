' **********************************
' List for sheet baseline
' **********************************
%read = "yes"
if %read = "yes" then

%list_com = "c_AZ c_DE c_C1 c_C2 c_C3 c_C4 c_C5 c_FZ c_GZ c_HZ c_IZ c_JZ c_KZ c_LZ c_MN c_OQ c_RU"

%show = "POP GDP_trend PARTR_trend" 
 
' Foreign prices for commodities
'for %c {%list_com}
'	%show = %show + " pwd_"+%c
'next

show  {%show}

endif

' **********************************
' List for sheet covid
' **********************************
%read2 = "no"
if %read2 = "yes" then

%list_com = "c_AZ c_DE c_C1 c_C2 c_C3 c_C4 c_C5 c_FZ c_GZ c_HZ c_IZ c_JZ c_KZ c_LZ c_MN c_OQ c_RU"

%show = ""  

' Shock on comsuption
for %c {%list_com}
	%show = %show + " shock_CH_"+%c
next
show  {%show}

endif


