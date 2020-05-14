' **********************************
' List for sheet baseline
' **********************************
%read = "no"
if %read = "yes" then

%list_com = "cAZ cDE cC1 cC2 cC3 cC4 cC5 cFZ cGZ cHZ cIZ cJZ cKZ cLZ cMN cOQ cRU"
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
%read2 = "yes"
if %read2 = "yes" then

%list_com = "cAZ cDE cC1 cC2 cC3 cC4 cC5 cFZ cGZ cHZ cIZ cJZ cKZ cLZ cMN cOQ cRU"

%show = ""  

' Shock on comsuption
for %c {%list_com}
	%show = %show + " shock_CH_"+%c
next

' Shock on investment
for %c {%list_com}
	%show = %show + " shock_I_"+%c
next

' Shock on goverment comsuption
for %c {%list_com}
	%show = %show + " shock_G_"+%c
next
' Shock on exports
for %c {%list_com}
	%show = %show + " shock_X_"+%c
next

show  {%show}

endif


