function averagetemp(tempstring,rawstring,AvgORSdev)
	string tempstring 				//Name of the wave containing the sorting key. Equal values in here get averaged.
	string rawstring					//Name of the wave containing the values to be averaged.
	variable AvgORSdev				//Variable (number) stating if the average, standard deviation or both should be calculated.
	
	wave rawwave=$rawstring		//Including the wave of the DataBrowser to the procedure.
	wave tempwave=$tempstring

	if(avgorsdev==0)					//Check if avg or sdev is chosen to determine the name of the results wave.
		make/o/N=1 $rawstring+"avg", $tempstring+"avg"	//Creating 2 result waves with 1 point.
		wave avgwave=$rawstring+"avg"							//Including the waves to the function.
		wave avgtemp=$tempstring+"avg"
	else
		make/o/n=1 $rawstring+"sdev", $tempstring+"sdev"
		wave avgwave=$rawstring+"sdev"
		wave avgtemp=$tempstring+"sdev"
	endif
	variable i, startavg=0, endavg=0, avgcounter=0, breaker=1,j, nancorrector		//Local variables just for averaging. Startavg and endavg are the point-numbers within the wave which should be averaged.
	for (i=0;i<numpnts(tempwave)-1;i+=1)			//i is used to go through the complete source waves. Numpnts gives the total length of tempwave.
		do													//No real do-loop. Only used to allow break-command inbetween.
	
		if(tempwave[i]==tempwave[i+1]&&i==0)		//1. Check if the first point (i==0) and (&&) the second point (tempwave[i]==tempwave[i+1]) of the sort wave are equal.
			endavg=1										//If so the current end of the average range is 1 (programmer start counting at 0). Dont do the rest below -> break
			break
		endif
		if(tempwave[i]==tempwave[i+1]&&i+1!=(numpnts(tempwave)-1))		//2. Check if the current and the next point (sorting wave) are equal and(&&) if the next point is NOT (!=) the last point of the wave (i+1!=(numpnts(tempwave)-1))
			endavg+=1			//If so the current end of the average range is increased by 1. Dont do the rest below -> break
			break
		endif
	
		if(tempwave[i]!=tempwave[i+1]||(i+1==(numpnts(tempwave)-1)&&tempwave[i]==tempwave[i+1]))	//If the next point IS NOT (!=) equal to the current OR if the next point is the last but they are equal -> Start average
			if(i+1==(numpnts(tempwave)-1)&&tempwave[i]==tempwave[i+1])		//If the last and the second last are equal the average-end (endavg) needs to be increased by one.
				endavg+=1
			endif
			
			if(AvgORSdev==0)					//Check wether the avg or sdev should be calculated.
				for(j=startavg;j<endavg+1;j+=1)		//Needed to ignore NaN in rawwaves. Would currupt the avg.
					if(numtype(rawwave[j])!=0)		//Check the value wave if there are NaNs (Not A Number)
						nancorrector+=1					//If there are NaNs the averaging range is reduced by 1.
					else
						avgwave[avgcounter]+=rawwave[j]	//If a value all values get consecuatively added to 1 row in the results wave.
					endif
				endfor
				avgwave[avgcounter]=avgwave[avgcounter]/(endavg-startavg+1-nancorrector)	//Before the sum of all values are in the row. Now divide the sum by the average range.
				nancorrector=0			//reset the nancounter variable for the next range to average.
			elseif(AvgORSdev==1||avgOrSdev==2)		//If sdev should be calcuated a 2nd function is called -> askforcorrecterror
				avgwave[avgcounter]=askforcorrecterror(rawstring,endavg,startavg,avgorsdev)
			else
				print "Wrong INPUT!!!"	//If a wrong avgorsdev value is provided print error message in history.
				Break
			endif
			//Now Avg or Sdev are calced. Now the values for the next run are adjusted.
			
			avgtemp[avgcounter]=tempwave[i]		//Corresponding to the avg/sdev value the sort key is saved in a separate wave.
			startavg=endavg+1						//Start point for next averaging is set to one higher than the end of the current run.
			endavg+=1									//End point for next averaging is increased by 1 == same value as new start point.
			avgcounter+=1							//The row where the results are saved is increased by one == new row
			if(i+1!=numpnts(tempwave)-1)			//If the next point in the raw wave IS NOT the last inseret a new row in the result waves.
				insertpoints numpnts(avgwave), 1, avgwave, avgtemp
			endif
			if(tempwave[i]!=tempwave[i+1]&&(i+1==(numpnts(tempwave)-1)))		//Special case: IF the last and second last value of the sort key wave are NOT EQUAL no averaging is possible!
				if(numtype(rawwave[i+1])==0)				//If the last value ALSO is number inseret a new row + fill with the single value.
					insertpoints numpnts(avgwave), 1, avgwave, avgtemp
				
					avgwave[avgcounter]=rawwave[i+1]
					avgtemp[avgcounter]=tempwave[i]
				endif
			endif 
			break		
		endif
		
		while(breaker<0)
	endfor
end

function askforcorrecterror(rawstring,endavg,startavg,avgorsdev)			//Function to calc average errors in different ways.
	string rawstring								//State the names and variables needed.
	variable endavg, startavg, avgorsdev
	wave rawwave=$rawstring					//Include the value wave.
	//Depending on the value of avgorsdev different calc methods are performed
	if(avgorsdev==1)								
		//Part where errorpropagation protocoll is done
		variable errorpropagation, u, nancorrector
		for(u=startavg;u<endavg+1;u+=1)		//Check within the averaging range all values.
			if(numtype(rawwave[u])!=0)		//If the values are NaN ignore.
				nancorrector+=1
			else
				errorpropagation+=rawwave[u]^2	//If they are a number calc the sum of all values squared.
			endif
		endfor
		errorpropagation=(errorpropagation)^0.5/(endavg-startavg-1-nancorrector)	//Standard formular for error propagation of an average value.
		nancorrector=0
		//Part where the standard deviation of the average value is calculated
//		rawstring=rawstring[0,strlen(rawstring)-6]			//Needed only for force-curve analysis.
	endif
	
	//Second way to calc sdev
	string rawstringvalues=rawstring//[0,strlen(rawstring)-6]
	wave rawwavetruevalues=$rawstringvalues	//klammer neu 170406
	variable average, sdev , sderror2
		//calc average value of all values for this range while ignoring NaNs
	for(u=startavg;u<endavg+1;u+=1)
		if(numtype(rawwavetruevalues[u])!=0)
			nancorrector+=1
		else
			average+=rawwavetruevalues[u]
		endif
	endfor
	average/=(endavg-startavg+1-nancorrector)		//==average
	//Calc the sum of all standard deviations squared
	for(u=startavg;u<endavg+1;u+=1)
		if(numtype(rawwavetruevalues[u])==0)
			sdev+=(rawwavetruevalues[u]-average)^2
		endif
	endfor
	//Sdev of Average value calculation
	sdev=(sdev/(endavg-startavg+1-nancorrector))^0.5
	//Standard error of average value calculation
	sderror2=sdev/(endavg-startavg+1-nancorrector)^0.5
	
	variable takethis			//This variable specifies which error should be saved in the end
	
	Variable/G takethis_g
	nvar takethis_g
	takethis=takethis_g
	make/N=2/O w_coef, w_sigma
	w_coef[0]={rawwavetruevalues[startavg],0}
	//Third methode to determine an error by Line-fitting with 0 slope and taking the error of the fit
	if(avgorsdev==1)
		CurveFit/H="01"/NTHR=0/Q line  rawwavetruevalues[startavg,endavg] /W=rawwave /I=1 /D 
	//	takethis=3
	else
		CurveFit/H="01"/NTHR=0/Q line  rawwavetruevalues[startavg,endavg] /D 
	//	takethis=3
	endif
	variable errorslope=w_sigma[0]

	// With prompt and doprompt user interaction is enforced by opening a window, displaying text and values and two buttons to close it
	//All calced errors with names and values are displayed
	prompt takethis, "Use 0 for Propagation, 1 for deviation, 2 for error, or 3 for line fit"	
	prompt errorpropagation, "Error from Errorpropagation"
	prompt sdev, "Standard Deviation"
	prompt sderror2, "Standard Error"
	prompt errorslope, "Error from Line Fit with sdev"
		
	doprompt "Which error to take?",takethis	, errorpropagation, sdev, sderror2, errorslope 	//by selecting a number the selected error-values is stated
	
	//with the return statement a value is transferred to the function above.
	if(takethis==0)
		return errorpropagation
	elseif(takethis==1)
		return sdev
	elseif(takethis==2)
		return sderror2
	elseif(takethis==3)
		return errorslope
	else
		print "Wrong input!"
	endif
	

end