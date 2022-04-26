/**********************************************************************

	Name: 01_data_aggregation.do
	Created: July, 2020
	Modified: September, 2020

	Purpose: This do-file runs through the data aggregation steps for 
	our N2FL NLP analysis, pulling text interaction data from across
	N2FL sites and harmonizing all variables as needed.
	
	NOTE: This file has been adjusted from the true analytic file
	to remove the identity of individual partner institutions. The
	nature and intention of the code within remain unchanged.

		
***********************************************************************/

/***********************************************************************

	#import - Import data. Start with Excel files, then do CSVs

***********************************************************************/

	/* 
	Checklist for when we add new institutions:
		1. Make sure student_id remains unique across institutions in the merged dataset
		2. Verify that the timecodes translate properly in the final cleaning steps in this file
		3. Verify counselor names can be pulled for their specific templates (02_advisor_flagging.R)
		4. Add to institution variable generation in 04_process_analytic_data.do
	*/

	//INST_A
		import delimited "${raw_data}/INST_A Data Files/Raw Data 07-28-2020/INST_A Deidentified message export.csv", bindquote(strict) clear
	
		rename customer_id student_id
		rename content_no_name text
		keep student_id template_id message_type time text
		gen institution="INST_A"
		tostring student_id, replace
	
		tempfile insta
		save `insta', replace
		
/***********************************************************************

	#clean - Merge and process datasets together

***********************************************************************/

	//Minor data cleaning
		drop if time==""
	
		gen new_time = clock(time, "YMDhms", 2030)
		format new_time %tc
		gen new_date = date(time, "YMDhms", 2030)
		format new_date %td
		
		replace new_time = clock(time, "MDYhm", 2030) if institution=="INST_A"
		replace new_date = date(time, "MDYhm", 2030) if institution=="INST_A"
	
		drop time
		rename new_time time
		rename new_date date
		
	//Modify the text variable format
		gen lengthtest=strlen(text)
		sum lengthtest, detail
		local maxlength=r(max)+20
		recast str`maxlength' text
		compress
		
	//Save it!
		save "${project_data}/01_combined.dta", replace
		export delimited using "${project_data}/01_combined.csv", replace quote
	
	