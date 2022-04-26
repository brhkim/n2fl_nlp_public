/* This file combines the student interaction data files, sentiment analysis files, and topic modeling files together after also calculating various opt-out phrases (please excuse the vulgarity!)

*/

*	Create a rough flag for opting-out
	use "${project_data}/04_interactions_cleaned.dta", clear
			gen optout = message_type=="received" & inlist(lower(text), "stop", "sotp", "stop.")
				replace optout = 1 if strpos(lower(text), "stop texting")!=0
				replace optout = 1 if strpos(lower(text), "stop contacting")!=0
				replace optout = 1 if strpos(lower(text), "quit texting")!=0
				replace optout = 1 if strpos(lower(text), "opt out")!=0
				replace optout = 1 if strpos(lower(text), "opt me out")!=0
				replace optout = 1 if strpos(lower(text), "wrong person")!=0
				replace optout = 1 if strpos(lower(text), "rong person")!=0
				replace optout = 1 if strpos(lower(text), "rong number")!=0
				replace optout = 1 if strpos(lower(text), "wrong number")!=0
				replace optout = 1 if strpos(lower(text), "remove me")!=0
				replace optout = 1 if strpos(lower(text), "remove my")!=0
				replace optout = 1 if strpos(lower(text), "leave me alone")!=0
				replace optout = 1 if strpos(lower(text), "unsubscribe")!=0
				replace optout = 1 if strpos(lower(text), "fuck off")!=0
				replace optout = 1 if strpos(lower(text), "fuck you")!=0
				replace optout = 1 if strpos(lower(text), "wrong phone")!=0
				replace optout = 1 if strpos(lower(text), "take me off")!=0
				replace optout = 1 if strpos(lower(text), "stop getting these messages")!=0
				replace optout = 1 if strpos(lower(text), "stop messaging me")!=0
				replace optout = 1 if strpos(lower(text), "don't text me")!=0
				replace optout = 1 if strpos(lower(text), "don't text this #")!=0
				replace optout = 1 if strpos(lower(text), "don't text this number")!=0
				replace optout = 1 if strpos(lower(text), "no more texts")!=0
				replace optout = 1 if strpos(lower(text), "please be removed")!=0
				replace optout = 1 if strpos(lower(text), "remove this number")!=0
				replace optout = 1 if strpos(lower(text), "stop notifying me")!=0
				replace optout = 1 if strpos(lower(text), "doesn't have this number anymore")!=0
				replace optout = 1 if strpos(lower(text), "sending me shit")!=0
				replace optout = 1 if strpos(lower(text), "do not want texts")!=0
				replace optout = 1 if strpos(lower(text), "please delete")!=0
				replace optout = 1 if strpos(lower(text), "end these messages")!=0
				replace optout = 1 if strpos(lower(text), "name's dad")!=0
				replace optout = 1 if strpos(lower(text), "name's mom")!=0
				replace optout = 1 if strpos(lower(text), "name is my")!=0
				replace optout = 1 if strpos(lower(text), "this is not name")!=0
				replace optout = 1 if strpos(lower(text), "shut up")!=0
				replace optout = 1 if num_form_texts<5
				
			egen ever_opt = max(optout), by(student_id)
				codebook student_id if ever_opt==1
				
			preserve
				egen keep = tag(student_id)
				keep if keep==1
				keep student_id ever_opt
				tempfile optout
				save `optout', replace
			restore
		clear
		
		
*Create conversation measures
use "${project_data}/04b_convosdata_woNA.dta", clear

	tempfile convos
	save `convos', replace

	
*Load in student data
use "${project_data}/04_interactions_cleaned.dta", clear
	merge m:1 student_id using "${project_data}/04_students_cleaned.dta"
	keep if treatment == 1 | treatment == 2 //two treatments in this variable

	keep if _m==3 //we have interactions and demographics
	capture drop _merge
	
*Merge topics
	merge m:1 student_id using `optout'
		keep if _m == 3 //skip the 84 misc.
		drop _m
	merge m:1 student_id using "${project_data}/04b_convosdata_woNA.dta"
		keep if _m == 3
		drop _m
	merge m:1 student_id using "${project_data}/05b_processed_topic_data.dta"
		keep if _m == 3
		drop _m

	drop if scheduled_message_count==. | scheduled_message_count==0
	//drop if ever_opt==1
	
	save "${project_data}/06_analysis_sample.dta", replace
	
