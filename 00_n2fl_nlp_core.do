/**********************************************************************

	Name: 00_n2fl_nlp_core.do
	Created: July, 2020
	Modified: July, 2020

	Purpose: This core do-file runs through the data aggregation, cleaning,
	and analytic steps for our N2FL NLP analysis by calling each of the
	component do-files in sequence.
	
	Do-files numbered normally are part of our main analysis, while
	do-files numbered at 99 are miscellaneous helper files for specific
	purposes (such as graphing for presentations or exploratory analyses).
		
***********************************************************************/

/***********************************************************************

	#setup - Setup file and prepare directory global

***********************************************************************/

	//Basics
		clear all
		set more off
	
	//Directory globals
		global username="`c(username)'"
		global root "/Users/${username}/Box Sync/N2FL NLP"
		global raw_data "/Users/${username}/Box Sync/Student Data Files for N2FL"
		global project_data "$root/data"
		global project_output "$root/output"
		global scripts "/Users/${username}/Box Sync/GitHub/n2fl_nlp"
		cd "$root"
	
	//Set logging file
		global sysdate=c(current_date)
		global sysdate=subinstr("$sysdate", " ", "", .)
		capture log close
		log using "${scripts}/logs/n2fl_nlp_log_$sysdate.log", replace
	
	//Analytic globals
		global mintextlength = 5
			//The minimum text length (in characters) we consider to be valid for analyzing sentiment.
			//All text lengths beneath this value have their sentiment scores removed from calculation
		global days_since_last = 3
	
	//Package dependencies
		capture ssc install vioplot
		capture ssc install mdesc
		//capture ssc install rscript
			//Required to run any of the R scripts below
	
	
	//Local run-switches for analytic code
		local switch_aggregation = 0
		local switch_advisor_flagging = 0
		local switch_validation_set_generator = 0
		local switch_validation_set_analysis = 0
		local switch_validation_set_algocompare = 0
		local switch_process_analytic_data = 0
		local switch_conversation_metrics = 0
		local switch_topic_model_training = 0
		local switch_topic_modeling = 0
		local switch_analytic_data_final = 0
		local switch_message_coding = 0
		local switch_design_analysis = 0
		local switch_tables_figures = 0
		
	
/***********************************************************************

	#run - Run various component do-files according to run-switches set above
	
***********************************************************************/

//Main Files
if `switch_aggregation' == 1 {
	do "${scripts}/01_data_aggregation.do"
}

if `switch_advisor_flagging' == 1 {
	rscript using "${scripts}/02_advisor_flagging.R"
}

	//SENTIMENT ANALYSIS SCRIPTS ARE TO BE RUN HERE (03x scripts)

if `switch_validation_set_generator' == 1 {
	rscript using "${scripts}/03d_validation_set_generator.R"
}

if `switch_validation_set_analysis' == 1 {
	rscript using "${scripts}/03e_validation_set_analysis.R"
}

if `switch_validation_set_algocompare' == 1 {
	rscript using "${scripts}/03f_validation_set_algocompare.R"
}

if `switch_process_analytic_data' == 1 {
	do "${scripts}/04_process_analytic_data.do"
}

if `switch_conversation_metrics' == 1 {
	rscript using "${scripts}/04b_conversation_metrics.R"
}

if `switch_topic_model_training' == 1 {
	rscript using "${scripts}/05a_topic_model_training.R"
}

if `switch_topic_modeling' == 1 {
	rscript using "${scripts}/05b_topic_modeling.R"
}

if `switch_analytic_data_final' == 1 {
	do "${scripts}/06_analytic_data_final.do"
}

if `switch_message_coding' == 1 {
	do "${scripts}/06b_message_coding.do"
}

if `switch_design_analysis' == 1 {
	do "${scripts}/06c_design_outcomes_analysis.do"
}

if `switch_tables_figures' == 1 {
	rscript using "${scripts}/07_tables_figures.R"
}
	
	
capture log close
