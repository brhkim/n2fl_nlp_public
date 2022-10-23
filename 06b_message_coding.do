/*******************************************************************************


CODES AND CREATES TABLE DESCRIBING THE MESSAGES STUDENTS RECEIVE



*******************************************************************************/

*	IMPORT coding
capture program drop import_codes

program import_codes

args school cohort sheetname system

		
		import excel "${project_data}/N2FL message coding\message_codes.xlsx", sheet("`sheetname'") firstrow case(lower) allstring clear

			replace a = a[_n-1] if a==""
			replace msg = msg[_n-1] if msg==""
			gen info = (strpos(content, "Infographic")>0)
			egen has_infographic = max(info), by(msg)
				capture drop info
				label var has_infographic "Message included an infographic"
				
			drop if content == "" //blank rows (doesn't get all of them)
			drop if content == "Insert MMS:"
			drop if strpos(content, "<Infographic")>0

			gen message_semester_first = strpos(a, "1st Semester")>0
			gen message_semester_second = strpos(a, "2nd Semester")>0
			gen message_semester_third = strpos(a, "3rd Semester")>0
			gen message_semester_fourth = strpos(a, "4th Semester")>0
			gen message_semester_fifth = strpos(a, "5th Semester")>0

			split a, p("-")
				gen message_semester = substr(a2, 2, 1)
				destring message_semester, replace
				label var message_semester "Sequence of semester messaged (e.g., first semester, second)"
		
			split a1, p(" ")
				gen message_term = a11+a12
					label var message_term "Academic term when messages distributed"
				rename a11 term_season
				rename a12 term_year
			capture drop a a1 a2 a13
			
			gen school = "`school'"
			gen cohort = "`cohort'"
			gen system = "`system'"
			
			gen template_id =`"""'+msg+`"""' //put in quotes to match template_id format
			
			egen keep = tag(template_id message_term) //template ID should be unique but be safe here
			keep if keep == 1
			capture drop keep
			
		save "${project_data}/N2FL message coding/`system'/`system'_`school'_`cohort'_codes.dta", replace

end

*CUNY
	import_codes "CCT" "C1" "CCT S18-S19 (C1)" "CUNY"
	import_codes "CCT" "C2" "CCT F18-S19 (C2)" "CUNY" //shows up as NYT in the college_short variable in "${project_data}/04_students_cleaned.dta"

	import_codes "SPS" "C1" "SPS S18-S19 (C1)" "CUNY"
	import_codes "SPS" "C2" "SPS F18-S19 (C2)" "CUNY"

	import_codes "Lehman" "C1" "Lehman F18-F19" "CUNY" //coded as LEH

	import_codes "KBCC" "C1" "KBCC S18-F18" "CUNY" //coded as KCC in the college_short
	
	import_codes "LAGCC" "C1" "LAGCC S18-S19" "CUNY" //LGCC in college_short
	
	import_codes "JJC" "C1" "JJC S18-S19 (C1)" "CUNY"
	import_codes "JJC" "C2" "JJC S19-F19 (C2)" "CUNY"
	
*VCCS
	import_codes "CVCC" "C1" "CVCC F18-F19" "VCCS"
	
	import_codes "JTCC" "C1" "JTCC S18-F19 (C1)" "VCCS"
	import_codes "JTCC" "C2" "JTCC S19-F19 (C2)" "VCCS"
	import_codes "GCC" "C1" "GCC S18-S19" "VCCS"

	import_codes "MECC" "C1" "MECC F18-F19" "VCCS"
	
	import_codes "PVCC" "C1" "PVCC F17-S19 (C1)" "VCCS"
	import_codes "PVCC" "C2" "PVCC S18-S19 (C2)" "VCCS"
	import_codes "PVCC" "C3" "PVCC F18-S19 (C3)" "VCCS"
	
	import_codes "TNCC" "C1" "TNCC F17-S19 (C1)" "VCCS"
	import_codes "TNCC" "C2" "TNCC S18-S19 (C2)" "VCCS"
	
	import_codes "WCC" "C1" "WCC F18-F19" "VCCS"

	
*TX	
	import_codes "ACC" "C1" "ACC F18-F19" "TX"
	import_codes "Alamo" "C1" "Alamo S19-F19" "TX"
	import_codes "Blinn" "C1" "Blinn F18-S19" "TX" //coded as BC in the college_short
	import_codes "UTA" "C1" "UTA F18-F19" "TX"
	import_codes "UTPB" "C1" "UTPB S18-S19" "TX"


*Append Codes
use "${project_data}/N2FL message coding/CUNY/CUNY_CCT_C1_codes.dta", clear
	foreach var in CCT_C2 SPS_C1 SPS_C2 Lehman_C1 KBCC_C1 LAGCC_C1 JJC_C1 JJC_C2 {
		append using "${project_data}/N2FL message coding/CUNY/CUNY_`var'_codes.dta"
	}
	foreach var in CVCC_C1 JTCC_C1 JTCC_C2 GCC_C1 MECC_C1 PVCC_C1 PVCC_C2 PVCC_C3 TNCC_C1 TNCC_C2 WCC_C1 {
		append using "${project_data}/N2FL message coding/VCCS/VCCS_`var'_codes.dta"
	}	
	foreach var in ACC_C1 Alamo_C1 Blinn_C1 UTA_C1 UTPB_C1 {
		append using "${project_data}/N2FL message coding/TX/TX_`var'_codes.dta"
	}	
	capture drop condition a a1 a2 a14 //all of these in named variables
	gen institution = school
	duplicates tag template_id cohort institution, gen(multicode)
	replace template_id = lower(template_id)
	replace template_id = subinstr(template_id, `"""', "", .)
		*Manual corrections
		replace template_id = "19_1" if template_id=="19.1" & institution=="UTPB"
		replace template_id = "12a_new" if template_id=="12a" & institution=="LAGCC"
		replace template_id = "12b_new" if template_id=="12b" & institution=="LAGCC"
		replace template_id = "13_new" if template_id=="13" & institution=="LAGCC"
		foreach id in 12 13 17 18 19 20 21 {
			replace template_id = "`id'_1" if template_id=="`id'.1" & institution=="PVCC"
		}
		replace template_id = "" if template_id=="" & institution==""
		replace template_id = "" if template_id=="" & institution==""
		replace template_id = "" if template_id=="" & institution==""

	
	*Expansions - example for one, need to scale
		expand 4 if template_id=="1a-1d" & institution=="LAGCC", gen(dupindicator)
			replace dupindicator=dupindicator[_n-1]+1 if template_id==template_id[_n-1] & institution==institution[_n-1]
			replace template_id = "1a" if template_id=="1a-1d" & institution=="LAGCC" & dupindicator==0
			replace template_id = "1b" if template_id=="1a-1d" & institution=="LAGCC" & dupindicator==1
			replace template_id = "1c" if template_id=="1a-1d" & institution=="LAGCC" & dupindicator==2
			replace template_id = "1d" if template_id=="1a-1d" & institution=="LAGCC" & dupindicator==3
		capture drop dupindicator
		
		foreach id in 1 21 23 {
		expand 10 if template_id=="`id'a-j" & institution=="GCC", gen(dupindicator)
			replace dupindicator=dupindicator[_n-1]+1 if template_id==template_id[_n-1] & institution==institution[_n-1]
			replace template_id = "`id'a" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==0
			replace template_id = "`id'b" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==1
			replace template_id = "`id'c" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==2
			replace template_id = "`id'd" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==3
			replace template_id = "`id'e" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==4
			replace template_id = "`id'f" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==5
			replace template_id = "`id'g" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==6
			replace template_id = "`id'h" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==7
			replace template_id = "`id'i" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==8
			replace template_id = "`id'j" if template_id=="`id'a-j" & institution=="GCC" & dupindicator==9
		capture drop dupindicator
		}
	
		foreach id in 10 2 3 4 5.1 5.2 6 7.1 7.2 8.1 8.2 9 {
		expand 4 if template_id=="`id'a-`id'd" & institution=="GCC" & cohort=="C1", gen(dupindicator)
			replace dupindicator=dupindicator[_n-1]+1 if template_id==template_id[_n-1] & institution==institution[_n-1]
			replace template_id = "`id'a" if template_id=="`id'a-`id'd" & institution=="LAGCC" & dupindicator==0 & cohort=="C1"
			replace template_id = "`id'b" if template_id=="`id'a-`id'd" & institution=="LAGCC" & dupindicator==1 & cohort=="C1"
			replace template_id = "`id'c" if template_id=="`id'a-`id'd" & institution=="LAGCC" & dupindicator==2 & cohort=="C1"
			replace template_id = "`id'd" if template_id=="`id'a-`id'd" & institution=="LAGCC" & dupindicator==3 & cohort=="C1"
		capture drop dupindicator
		}
		
		
	*Clean up categories
		*Category 1 - Topics
		tab category1, m
			replace category1 = "4e and 4d" if category1=="4e ad 4d" //fucking hard coding shit
			split category1, p("and")
			split category11, p(",")
			drop category11 //original/non-split
			foreach var in category12 category111 category112 category113 {
				replace `var' = subinstr(`var', " ", "", .)
				replace `var' = subinstr(`var', "  ", "", .)
				replace `var' = subinstr(`var', "13 ", "13", .) //WHY THE FUCK is that not working with the above lines?
				foreach subgroup in a b c d e f {
					replace `var' = "4`subgroup'" if `var'=="`subgroup'"
				}
			}
			
			*PROGRAM: For all the possible topic flags, search through all the variables flagging a message
			capture program drop topic_naming
			program topic_naming
				args topicname topicflag

			gen topic_`topicname' = category12=="`topicflag'"
				foreach var in category111 category112 category113 {
					replace topic_`topicname' = 1 if `var' == "`topicflag'"
				}
				
			end
			
			topic_naming "intro" "1"
			topic_naming "transition" "2"
			topic_naming "checkin" "3"
			topic_naming "fafsa" "5"
			topic_naming "loans" "6"
			topic_naming "aidstatud" "7"
			topic_naming "academicsupport" "8"
			topic_naming "gradplan" "9"
			topic_naming "financialmgmt" "10"
			topic_naming "paytuition" "11"
			topic_naming "applygrad" "12"
			topic_naming "other" "13"
			topic_naming "endofsemester" "14"
			
			topic_naming "reg_appoint" "4a"
			topic_naming "reg_dates" "4b"
			topic_naming "reg_holds" "4c"
			topic_naming "reg_degreereq" "4d"
			topic_naming "reg_addcourse" "4e"
			topic_naming "reg_early" "4f"
				gen topic_registration = 0
					foreach var in category111 category112 category113 {
						replace topic_registration = 1 if strpos(`var', "4")!=0
					}
								
			gen supertopic_bookend = (topic_intro==1 | topic_transition==1 | topic_endofsemester==1)
			gen supertopic_registration = topic_registration==1
			gen supertopic_financial = (topic_fafsa==1 | topic_loans==1 | topic_aidstatud==1 | topic_financialmgmt==1 | topic_paytuition==1)
			gen supertopic_academic = (topic_academicsupport==1 | topic_applygrad==1 | topic_gradplan==1)
			gen supertopic_other = topic_other==1
			gen supertopic_checkin = topic_checkin==1
		egen totalsupers = rowtotal(supertopic*)	
		*Category 2 - Framing
		tab category2, m
			*Don't have double digits, so actually ok with strpos?
			gen frame_direct = strpos(category2, "1")!=0
			gen frame_connection = strpos(category2, "2")!=0
			gen frame_activechoice = strpos(category2, "3")!=0
			gen frame_lossaversion = strpos(category2, "4")!=0
			gen frame_commitmentdevice = strpos(category2, "5")!=0
			gen frame_planprompt = strpos(category2, "6")!=0
			gen frame_socialnorm = strpos(category2, "7")!=0
			gen frame_na = strpos(category2, "8")!=0
		egen totalframes = rowtotal(frame*)
		
		*Category 3 - Timing
		tab category3, m
			gen time_twoplus = strpos(category3, "1")!=0
			gen time_onetotwo = strpos(category3, "2")!=0
			gen time_oneless = strpos(category3, "3")!=0
			gen time_na = strpos(category3, "4")!=0
		egen totaltimes = rowtotal(time*)
		*Category 4 - Sequence
		tab category4, m
			foreach sem in 1 2 3 4 5 {
				gen sequence_sem`sem' = strpos(category4, "`sem'")!=0
			}
		egen totalseq = rowtotal(seq*)
		
		*Immediacy effect
		gen sequence_early = strpos(msg, "1")!=0 & strpos(msg, "11")==0 & strpos(msg, "12")==0 & strpos(msg, "13")==0 & strpos(msg, "14")==0 & strpos(msg, "15")==0 & strpos(msg, "16")==0 & strpos(msg, "17")==0 & strpos(msg, "18")==0 & strpos(msg, "19")==0 & sequence_sem1==1
		foreach num in 2 3 4 5 6 7 8 9 {
			replace sequence_early = 1 if strpos(msg, "`num'")!=0 & strpos(msg, "1`num'")==0 & sequence_sem1==1
		}
		
		replace institution = "CT" if institution == "CCT"
		drop if template_id=="" //this blank row is incorrectly merging to student messages 
		
		su totalsupers totalframes totaltimes totalseq
		
save "${project_data}/N2FL message coding/message_codes.dta", replace
	
	
*Table: Scheduled message Characteristics
use "${project_data}\N2FL message coding/message_codes.dta", clear
	preserve
		local rows = 500
		local cols = 500

		matrix results = J(`rows', `cols', 0)
		local col = 1
		local row = 1	

		foreach var in supertopic_bookend supertopic_registration supertopic_financial supertopic_academic supertopic_checkin supertopic_other  frame_direct frame_connection frame_activechoice frame_lossaversion frame_commitmentdevice frame_planprompt frame_socialnorm frame_na time_twoplus time_onetotwo time_oneless time_na {
		local col = 1
			
			su `var'
				matrix results[`row', `col'] = r(mean)
			loc ++row
			}				
		
		clear
		svmat results
		export excel "${project_output}/06d_paper_tables.xlsx", sheet(msg_codes) sheetreplace 
	restore		

*Example messages
use "${project_data}\N2FL message coding/message_codes.dta", clear

foreach var in supertopic_bookend supertopic_registration supertopic_financial supertopic_academic supertopic_checkin supertopic_other  frame_direct frame_connection frame_activechoice frame_lossaversion frame_commitmentdevice frame_planprompt frame_socialnorm frame_na time_twoplus time_onetotwo time_oneless time_na {

preserve
	keep if `var' == 1
	keep content
	gen code = "`var'"
	keep in 10
	order code content 
	
	tempfile file_`var'
	save `file_`var''
restore
}

use `file_supertopic_bookend', clear
foreach var in supertopic_registration supertopic_financial supertopic_academic supertopic_checkin supertopic_other  frame_direct frame_connection frame_activechoice frame_lossaversion frame_commitmentdevice frame_planprompt frame_socialnorm frame_na time_twoplus time_onetotwo time_oneless time_na {
	append using `file_`var''
}
save "${project_data}/N2FL message coding/example_templates.dta", replace
