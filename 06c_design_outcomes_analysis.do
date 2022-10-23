/**********************************************************************

	Name: RENAME 
	Created: August, 2022
	Modified: August, 2022

	Purpose: This file examines how message design relates to student engagement measures
	
***********************************************************************/

*CLEAN FOR ANALYSIS 

*	Merge message codes to student interaction data
	*Open the student list
	use "${project_data}/04_students_cleaned.dta", clear
	drop if treatment == 0 //drop control students
		*ID launch/cohort variable for merge
		sort college_short launch
			egen cohort_tag = tag(college_short launch)
			gsort college_short -cohort_tag launch
				replace cohort_tag = cohort_tag[_n-1]+1 if college_short==college_short[_n-1] & cohort_tag>0
		gsort college_short launch -cohort_tag
			replace cohort_tag=cohort_tag[_n-1] if cohort_tag==0 & college_short==college_short[_n-1] 
		gen cohort = "C1" if cohort_tag==1
			replace cohort = "C2" if cohort_tag==2
			replace cohort = "C3" if cohort_tag==3
	merge 1:m student_id using "${project_data}/06d_analysis_sample.dta", gen(message_merge)
		codebook student_id if message_merge==3
		keep if message_merge==3 //n=10102 like the main analysis; 87 students dropped from start
		gen ever_replied = scheduled_replies!=0
		gen never_replied = scheduled_replies==0
		
		codebook student_id
	
	*Consistent cleaning
		replace template_id = lower(template_id)
		replace template_id = subinstr(template_id, `"""', "", .)
		replace template_id = subinstr(template_id, " ", "", .)
	merge m:1 template_id cohort institution using "${project_data}/N2FL message coding/message_codes.dta", gen(code_merge)
		tab institution cohort if code_merge==2, m
		
		drop if code_merge==2 //187 mostly from CCT don't merge from coding
		
		tab institution code_merge if message_type=="sent" & template_id!="", m row //what messages aren't merging?	
		
			gen custom_advisor = strpos(template_id, "qm")!=0
			tab custom_advisor code_merge if message_type=="sent" & template_id!="", m col //how many "qm" messages?
		
	sort student_id time
	gen coded_message = code_merge==3
		
save "${project_data}/engagement_predictor_long.dta", replace

*CREATE STUDENT-LEVEL EXPOSURE DATA	
use "${project_data}/engagement_predictor_long.dta", clear
	drop if ever_opt == 1
	keep if ever_replied == 1
	keep if coded_message == 1 //only looking at template shares for student-level data
	
	loc demographics age_entry male white afam hisp other race_missing risk_rating
	
	loc baseline credits_earned_total_bline coll_gpa_cum_bline prop_comp_bline credits_att_last_term_bline term_gpa_last_bline transferee_ever_bline prior_stopout_ever_bline major_pursued_chng_ever_bline terms_enrolled_bline degree_intent_BA_bline
	
	loc engagement scheduled_reply_proportion avg_convo_length_student_msgs avg_convo_length_student_words substantive_reply_prop prop_msgs_to_engagement prop_msgs_to_lastengagement engagement_duration_prop_msgs scheduled_response_time all_response_time help_messages_prop received_text_sentiment received_text_crse_prop received_text_fina_prop received_text_plan_prop received_text_reso_prop received_text_meet_prop
	
	loc message_design supertopic_bookend supertopic_registration supertopic_financial supertopic_academic supertopic_checkin supertopic_other frame_direct frame_connection frame_activechoice frame_lossaversion frame_commitmentdevice frame_planprompt frame_socialnorm frame_na time_twoplus time_onetotwo time_oneless time_na	

	loc outcomes term1_credits_earned term1_gpa enrl1 degree_any1 //really just to test
	
	collapse (mean) `message_design' (first) block system college launch `engagement' `baseline' `demographics' `outcomes' (sum) coded_message, by(student_id)
		isid student_id
	
	save "${project_data}/messages_received_collapsed.dta", replace


*ANALYSIS - Distribution of messages received
use "${project_data}/messages_received_collapsed.dta", clear

	*Sample figure of variance
	su supertopic_bookend, det
		loc med: di %5.3f r(p50)
	kdensity supertopic_bookend, title("Share of Bookend Messages Received") xtitle("Share of Bookend Messages") xline(`med')
		graph export "${project_output}/density_bookend.png", as(png) replace


*ANALYSIS - Relationship between exposure and engaement
use "${project_data}/messages_received_collapsed.dta", clear

*VERY LARGE REGRESSION TABLE
	preserve
		local rows = 500
		local cols = 500

		matrix results = J(`rows', `cols', 0)
		local col = 1
		local row = 1	

		loc demographics age_entry male afam hisp other race_missing risk_rating

		loc baseline credits_earned_total_bline coll_gpa_cum_bline terms_enrolled_bline major_pursued_chng_ever_bline prior_stopout_ever_bline transferee_ever_bline

		*MEAN
		foreach engagement_measure in scheduled_reply_proportion avg_convo_length_student_msgs avg_convo_length_student_words substantive_reply_prop prop_msgs_to_engagement prop_msgs_to_lastengagement engagement_duration_prop_msgs scheduled_response_time all_response_time help_messages_prop received_text_sentiment received_text_crse_prop received_text_fina_prop received_text_plan_prop received_text_reso_prop received_text_meet_prop {
			su `engagement_measure'
				matrix results[`row', `col'] = r(mean)
				matrix results[`row'+1, `col'] = r(sd)
					loc ++col
					loc ++col			
			}
			loc ++row
			loc ++row

		*REGRESSION
		*FOREACH X
		foreach template_measure in supertopic_bookend supertopic_registration supertopic_financial supertopic_academic supertopic_checkin supertopic_other  frame_direct frame_connection frame_activechoice frame_lossaversion frame_commitmentdevice frame_planprompt frame_socialnorm frame_na time_twoplus time_onetotwo time_oneless time_na {
		loc col = 1	//start in the first column
			
			*FOREACH Y
			foreach engagement_measure in scheduled_reply_proportion avg_convo_length_student_msgs avg_convo_length_student_words substantive_reply_prop prop_msgs_to_engagement prop_msgs_to_lastengagement engagement_duration_prop_msgs scheduled_response_time all_response_time help_messages_prop received_text_sentiment received_text_crse_prop received_text_fina_prop received_text_plan_prop received_text_reso_prop received_text_meet_prop {
				areg `engagement_measure' `template_measure' `demographics' `baseline' i.launch, r cluster(system) a(system)
						matrix results[`row', `col'] = _b[`template_measure']
						matrix results[`row'+1, `col'] = _se[`template_measure']
						matrix results[`row', `col'+1] = _b[`template_measure']/_se[`template_measure']
					loc ++col
					loc ++col
				}
			loc ++row
			loc ++row
		}
		clear
		svmat results
		export excel "${project_output}/06d_paper_tables.xlsx", sheet(msg_regress) sheetreplace 
	restore		

	
	
	