/**********************************************************************

	Name: 04_process_analytic_data.do
	Created: August, 2020
	Modified: September, 2020

	Purpose: This do-file intakes the sentiment analysis output files
	and merges them with outcomes and
	covariate data from the rest of the N2FL intervention database
	
	
***********************************************************************/

	//Load main data
		use "${project_data}/02_advisornames_cleaned.dta", clear
		
		egen advisor_id = group(institution advisorname) if advisorname!=""
		
	//Clean out duplicate texts before constructing measures
		gsort student_id time text
		by student_id: gen duplicate=(text==text[_n-1] & time==time[_n-1])
		drop if duplicate==1
		drop duplicate
		
	//Restrict to texts within one year of intervention starting
		egen first_scheduled_date_prep=min(date) if template_id!="", by(student_id)
		egen first_scheduled_date=mean(first_scheduled_date_prep), by(student_id)
		egen last_scheduled_date_prep=max(date) if template_id!="", by(student_id)
		egen last_scheduled_date=mean(last_scheduled_date_prep), by(student_id)
		
		format first_scheduled_date %td
		format last_scheduled_date %td
		drop if date>(last_scheduled_date+14)
		
	//Merge in forest output
		preserve
			import delimited "${project_data}/03c_forest_output_masked.csv", clear encoding(utf-8)
			
			//Minor variable cleaning
				drop v1 merge index yelp_* xlnet_* albert_* stanza_*
				gen lengthtest=strlen(text)
				sum lengthtest, detail
				local lengthmax=r(max)
				recast str`lengthmax' text
				
			tempfile temp
			save `temp', replace
		restore
		
		merge m:1 text using `temp', gen(sentiment_merge)
		
		gsort institution student_id time
		
	//Destring the prediction scores for quant analysis
		foreach var of varlist label_predict_* {
			replace `var'=substr(`var', 7, 1)
			destring `var', replace
			replace `var' = `var'-2
		}
		
	//Flag custom texts for analysis groupings
		gen text_length = strlen(text)
		gen legit_text = (text_length >= $mintextlength & !missing(text_length))
		
		gen custom_text = .
		replace custom_text = 1 if template_id=="" & message_type=="sent" & legit_text==1
		replace custom_text = 0 if template_id!="" & message_type=="sent" & legit_text==1
			
	//Get average sentiment scores for form texts only within each student
		egen num_form_texts_prep = total(1-custom_text) if !missing(custom_text), by(student_id)
		egen num_form_texts = mean(num_form_texts_prep), by(student_id)
		
		egen form_text_sentiment_prep = mean(label_predict_forest) if custom_text==0, by(student_id)
		egen form_text_sentiment = mean(form_text_sentiment_prep), by(student_id)
		
		egen form_text_length_prep=total(text_length/5) if custom_text==0, by(student_id)
		egen form_text_length=mean(form_text_length_prep), by(student_id)
	
	//Get average sentiment scores for non-form texts only within each student
		egen num_nonform_texts_prep = total(custom_text) if !missing(custom_text), by(student_id)
		egen num_nonform_texts=mean(num_nonform_texts_prep), by(student_id)
		
		egen nonform_text_sentiment_prep = mean(label_predict_forest) if custom_text==1, by(student_id)
		egen nonform_text_sentiment=mean(nonform_text_sentiment_prep), by(student_id)
		
		egen nonform_text_length_prep=total(text_length/5) if custom_text==1, by(student_id)
		egen nonform_text_length=mean(nonform_text_length_prep), by(student_id)
	
	//Get average sentiment scores across all texts sent by advisors within each student
		egen num_sent_texts_prep = total(legit_text) if !missing(legit_text) & message_type=="sent", by(student_id)
		egen num_sent_texts=mean(num_sent_texts_prep), by(student_id)
		
		egen sent_text_sentiment_prep = mean(label_predict_forest) if legit_text==1 & message_type=="sent", by(student_id)
		egen sent_text_sentiment=mean(sent_text_sentiment_prep), by(student_id)
		
		egen sent_text_length_prep=total(text_length/5) if legit_text==1 & message_type=="sent", by(student_id)
		egen sent_text_length=mean(sent_text_length_prep), by(student_id)
		
	//Get average sentiment scores across all texts received by advisors within each student
		egen num_received_texts_prep = total(legit_text) if !missing(legit_text) & message_type=="received", by(student_id)
		egen num_received_texts=mean(num_received_texts_prep), by(student_id)
		
		egen received_text_sentiment_prep = mean(label_predict_forest) if legit_text==1 & message_type=="received", by(student_id)
		egen received_text_sentiment=mean(received_text_sentiment_prep), by(student_id)
		
		egen received_text_length_prep=total(text_length/5) if legit_text==1 & message_type=="received", by(student_id)
		egen received_text_length=mean(received_text_length_prep), by(student_id)
	
	//Get average sentiment scores across all texts in the conversation
		egen num_all_texts_prep = total(legit_text) if !missing(legit_text), by(student_id)
		egen num_all_texts=mean(num_all_texts_prep), by(student_id)
		
		egen all_text_sentiment_prep = mean(label_predict_forest) if legit_text==1, by(student_id)
		egen all_text_sentiment=mean(all_text_sentiment_prep), by(student_id)
		
		egen all_text_length_prep=total(text_length/5) if legit_text==1, by(student_id)
		egen all_text_length=mean(all_text_length_prep), by(student_id)
	
	// Drop prepvars
		drop form_text_sentiment_prep form_text_length_prep num_form_texts_prep
		drop nonform_text_sentiment_prep nonform_text_length_prep num_nonform_texts_prep
		drop sent_text_sentiment_prep sent_text_length_prep num_sent_texts_prep
		drop received_text_sentiment_prep received_text_length_prep num_received_texts_prep
		drop all_text_sentiment_prep all_text_length_prep num_all_texts_prep
	
	//Flag for advisor switching
		egen new_advisor = tag(student_id advisorname)
		egen num_advisors = total(new_advisor), by(student_id)
		gen switched_advisors = (num_advisors>1 & !missing(num_advisors))
		
	//Measure number of discrete conversations
		gsort student_id time
		
		by student_id: gen days_since_last=(date-date[_n-1])
		by student_id: gen mins_since_last=((time-time[_n-1])/60000)
		gen consecutive=0
		replace consecutive=1 if days_since_last<=$days_since_last
		replace consecutive=0 if template_id!=""
		
		by student_id: gen conversations=(consecutive==0 & consecutive[_n+1]==1)
		//drop consecutive
		
	//Generate a unique ID per text message for ease of identification later
		gen text_id = _n
		
	//Save cleaned interaction-level dataset first
		save "${project_data}/04_interactions_cleaned.dta", replace
		
	//Collapse for student-level analyses
		use "${project_data}/04_interactions_cleaned.dta", clear //KM issue running code above

		collapse (mean) num_form_texts form_text_sentiment form_text_length /// 
			num_nonform_texts nonform_text_sentiment nonform_text_length ///
			num_sent_texts sent_text_sentiment sent_text_length ///
			num_received_texts received_text_sentiment received_text_length ///
			num_all_texts all_text_sentiment all_text_length switched_advisors ///
			(sum) conversations, by(student_id)
			
	//Merge in outcomes/covariate data
		preserve
			use "${raw_data}/Scale Phase Analysis/data/clean/n2fl_sphase_analytic_sample_with_outcomes_052021.dta", clear
			
			//Minor cleaning to prep for merge
			gen institution=""
			replace institution="INST_A"
			rename stuid student_id
			
			tempfile temp
			save `temp', replace
		restore
		
		merge m:1 student_id using `temp', gen(main_merge)

	//Calculate quintiles of texting amounts
		foreach var of varlist num_form_texts num_nonform_texts num_sent_texts num_received_texts num_all_texts {
		    xtile `var'_5tile = `var' if `var'>0 & !missing(`var'), n(5)
		}
		
		
	//Set sample
		//Only currently merged institutions
			drop if institution==""
	
		//Individual weird observation existing at both Blinn and Alamo?
			drop if student_id=="314649"
			
		//Individual erroneous student_id
			drop if student_id=="1"
			
	//Flag advising models (do here instead of later?)
		gen model = ""
			replace model = "professional" if inlist(institution, "INST_A")
		
	//Save the dataset
		save "${project_data}/04_students_cleaned.dta", replace

		