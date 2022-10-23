################################################################################
#  
#   Name: 07_tables_figures.R
#   
#   Purpose: Create simple descriptives tables for the N2FL text analysis
#
################################################################################


################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

pacman::p_load(tidyverse, tidylog, haven, lubridate, furrr, ggthemes, 
               ggridges, corrplot, xlsx, kableExtra, fixest)

username <- Sys.getenv("USERNAME")
home <- Sys.getenv("HOME")

setwd(home)

root <- file.path('..','Box Sync', 'N2FL NLP')
project_data <- file.path(root, 'data')
project_output <- file.path(root, 'output')


################################################################################
#
#   #import - Load in a dataset and start taking a peek around
#
################################################################################

## Load in the data
scheduled_data <- read_dta(file.path(project_data, 'messages_received_collapsed.dta')) %>%
  select(supertopic_bookend, supertopic_registration, supertopic_financial, supertopic_academic)

data <- read_dta(file.path(project_data, '06_analysis_sample.dta')) 


################################################################################
#
#   #visualize - Make a nice output table for descriptives
#
################################################################################

# get a list of demographic variables together for the table
demoslist <- c("student_id", "age_entry", "male", "white", "afam", "hisp", "other", "race_missing",
               "credits_earned_total_bline", "coll_gpa_cum_bline", "terms_enrolled_bline", 
               "prior_stopout_ever_bline", "major_pursued_chng_ever_bline", "transferee_ever_bline",  
               "system", "scheduled_replies", "ever_opt")

demosdata <- data %>%
  group_by(student_id) %>%
  ungroup() %>%
  select(all_of(demoslist)) %>%
  distinct()

descriptives_subchain <- . %>%
  summarize(`N` = nrow(.),
            `Age at Entry` = mean(age_entry),
            Male = mean(male),
            White = mean(white),
            Black = mean(afam),
            Hispanic = mean(hisp),
            `Other Race` = mean(other),
            `Missing Race` = mean(race_missing),
            `Credits Earned` = mean(credits_earned_total_bline),
            `Cumulative GPA` = mean(coll_gpa_cum_bline, na.rm=TRUE),
            `Terms Enrolled` = mean(terms_enrolled_bline),
            `Any Prior Stopout` = mean(prior_stopout_ever_bline),
            `Any Prior Change of Major` = mean(major_pursued_chng_ever_bline),
            `Any Prior Transfer` = mean(transferee_ever_bline),
            `VCCS` = length(.$system[.$system=="VCCS"])/length(.$system),
            `CUNY` = length(.$system[.$system=="CUNY"])/length(.$system),
            `TX` = length(.$system[.$system=="TX"])/length(.$system),
            ) %>%
  stack() %>%
  relocate(ind, values) %>%
  rename(Variable=ind, Values=values) %>%
  mutate(Values=as.character(round(Values, digits=2)))

# Start making separate datasets for descriptives for each group
sample_descriptives_study <- demosdata %>%
  filter(ever_opt==0 & scheduled_replies>0) %>%
  descriptives_subchain() %>%
  rename(`Study Sample` = Values)

sample_descriptives_optouts <- demosdata %>%
  filter(ever_opt==1) %>%
  descriptives_subchain() %>%
  rename(`Opt-Outs`= Values)

sample_descriptives_nonresponders <- demosdata %>%
  filter(scheduled_replies==0 & ever_opt==0) %>%
  descriptives_subchain() %>%
  rename(`Non-Responders`=Values)

# Get them all together
sample_descriptives <- bind_cols(sample_descriptives_study,
                                 sample_descriptives_nonresponders,
                                 sample_descriptives_optouts) %>%
  select(-Variable...3, -Variable...5) %>%
  rename(Variable=Variable...1)

# print the table nicely
kable(sample_descriptives, 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'", background="white" ) %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Sample", 1, 3) %>%
  pack_rows("Race/Ethnicity", 4, 8) %>%
  pack_rows("Academics at Baseline", 9, 14) %>%
  pack_rows("System", 15, 17) %>%
  save_kable(file.path(project_output, "sample_descriptives_bk.jpg"), zoom=2, bs_theme="default")


# Get lists together for relevant outcomes, engagement variables, and labels for the variables
outcomes_list <- c("enrl1", "term1_credits_earned", "term1_gpa", "degree_any1",
                   "enrl2", "term2_credits_earned", "term2_gpa", "degree_any2",
                   "enrl3", "term3_credits_earned", "term3_gpa", "degree_any3",
                   "enrl4", "term4_credits_earned", "term4_gpa", "degree_any4")

variables_list <- c("scheduled_reply_proportion",
                    
                    "avg_convo_length_student_msgs", "avg_reply_length_words", "substantive_reply_prop",
                    
                    "prop_msgs_to_engagement", "prop_msgs_to_lastengagement",
                    "engagement_duration_prop_msgs", 
                    
                    "scheduled_response_time", "all_response_time", 
                    
                    
                    "help_messages_prop", "received_text_sentiment",
                    
                    "received_text_crse_prop", "received_text_fina_prop", "received_text_plan_prop", "received_text_reso_prop",
                    "received_text_meet_prop")

labels <- c(
  "% of Scheduled Messages Responded to",
  "Average Replies Per Scheduled Message",
  "Average Reply Length in Words",
  "Proportion of Substantive Replies",
  "% of Scheduled Messages Before Engagement",
  "% of Scheduled Messages Before Disengagement",
  "% of Scheduled Messages Engaged",
  "Response Time to Scheduled Messages (Hours)",
  "Response Time to Any Advising Messages (Hours)",
  "Proportion of Messages Asking for Help",
  "Average Sentiment of Replies",
  "% of Replies About Course Planning",
  "% of Replies About Financial Aid",
  "% of Replies About Academic Planning",
  "% of Replies About Academic Support",
  "% of Replies About Meeting Logistics")

# Make a dataset for the density plots
densitydata <- data %>%
  filter(ever_opt==0 & scheduled_replies>0) %>%
  group_by(student_id) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(all_of(variables_list))

# Loop over the density plots for each engagement variable
j <- 1
for(i in names(densitydata)) {
  nsize <- densitydata %>%
    filter(!is.na(.[[i]])) %>%
    nrow()
  median <- densitydata %>%
    filter(!is.na(.[[i]])) %>%
    .[[i]] %>%
    median() %>%
    round(digits=2)
  mean <- densitydata %>%
    filter(!is.na(.[[i]])) %>%
    .[[i]] %>%
    mean() %>%
    round(digits=2)
  densitydata %>%
    ggplot(aes(x=.data[[i]])) +
    geom_density(adjust=1/2) +
    labs(x=labels[j], y="Density\n", title=paste0("Distribution of ", labels[j]),
         subtitle=paste0(" (N = ", nsize, ") (Mean = ", mean, ") (Median = ", median, ")")) + 
    geom_vline(xintercept=median) + 
    geom_vline(xintercept=mean, linetype="dashed") + 
    theme_minimal(base_size=20)
    
  ggsave(filename=file.path(project_output, paste0("99_tables_", j, "_", i, ".png")), width=12, height=4, dpi=500, bg="white")
    
  j <- j + 1
}

# Loop over the density plots for each scheduled message code variable
scheduled_labels <- c("% of Received Scheduled Messages: Bookend",
                      "% of Received Scheduled Messages: Registration",
                      "% of Received Scheduled Messages: Financial Matters",
                      "% of Received Scheduled Messages: Academic Supports")

j <- 1
for(i in names(scheduled_data)) {
  nsize <- scheduled_data %>%
    filter(!is.na(.[[i]])) %>%
    nrow()
  median <- scheduled_data %>%
    filter(!is.na(.[[i]])) %>%
    .[[i]] %>%
    median() %>%
    round(digits=2)
  mean <- scheduled_data %>%
    filter(!is.na(.[[i]])) %>%
    .[[i]] %>%
    mean() %>%
    round(digits=2)
  scheduled_data %>%
    ggplot(aes(x=.data[[i]])) +
    geom_density(adjust=1/2) +
    labs(x=scheduled_labels[j], y="Density\n", title=paste0("Distribution of ", scheduled_labels[j]),
         subtitle=paste0(" (N = ", nsize, ") (Mean = ", mean, ") (Median = ", median, ")")) + 
    geom_vline(xintercept=median) + 
    geom_vline(xintercept=mean, linetype="dashed") + 
    theme_minimal(base_size=20)
  
  ggsave(filename=file.path(project_output, paste0("99_tables_scheduled_", j, "_", i, ".png")), width=12, height=4, dpi=500, bg="white")
  
  j <- j + 1
}

# Make another scatterplot for the engagement and disengagement variables
set.seed(1234)
densitydata %>%
  ggplot(aes(x=prop_msgs_to_engagement, y=prop_msgs_to_lastengagement)) +
  geom_point(position=position_jitter(width=0.01, height=0.01), alpha=0.2, shape=16) + 
  labs(y="% of Scheduled Messages Before Disengagement\n", x="% of Scheduled Messages Before Engagement", title=paste0("Engagement and Disengagement By Student:"), subtitle="Proportion of Scheduled Messages") +
  theme_minimal(base_size=20)

ggsave(filename=file.path(project_output, paste0("99_engagement_disengagement_scatter_scheduled.png")), width=12, height=8, dpi=500, bg="white")

# Load Hmisc again to run the correlation matrices
pacman::p_load(Hmisc)

# Get the data in the format for the correlation matrices
corrdata <- data %>%
  filter(ever_opt==0) %>%
  filter(scheduled_replies>0) %>%
  group_by(student_id) %>%
  mutate(received_text_sentiment=mean(received_text_sentiment, na.rm=TRUE)) %>%
  slice_head(n=1) %>%
  select(all_of(outcomes_list), all_of(variables_list)) %>%
  ungroup() %>%
  select(-student_id)

corrout <- rcorr(as.matrix(corrdata))

# Create a function to make the correlation matrix object more amenable to output
flattenCorrMatrix <- function(cormat, pmat, nmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut],
    n = nmat[ut]
  )
}

output <- flattenCorrMatrix(corrout$r, corrout$P, corrout$n)

corrplot(corrout$r, type="lower", method="color", tl.cex=0.6)

rownames(corrout$r)

# Write out the xlsx files for each parameter of the correlation matrices
write.xlsx(corrout$r, file=file.path(project_output, "engagement_correlation_matrix.xlsx"), sheet="raw_correlation")
write.xlsx(corrout$n, file=file.path(project_output, "engagement_correlation_matrix.xlsx"), sheet="raw_n", append=TRUE)
write.xlsx(corrout$P, file=file.path(project_output, "engagement_correlation_matrix.xlsx"), sheet="raw_P", append=TRUE)

# Run simple controlled regressions on each outcome
controls_list <- c("student_id", "block", "age_entry", "male", "afam", "hisp", "other", "race_missing",
                   "credits_earned_total_bline", "coll_gpa_cum_bline", "terms_enrolled_bline",
                   "major_pursued_chng_ever_bline",  "prior_stopout_ever_bline", "transferee_ever_bline",  "system")

regressiondata <- data %>%
  filter(ever_opt==0 & scheduled_replies>0) %>%
  group_by(student_id) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(all_of(variables_list), all_of(outcomes_list), all_of(controls_list)) %>%
  distinct()

test <- feols(data=regressiondata, fml=.[outcomes_list] ~ 
                age_entry + male + afam + hisp + other + race_missing +
                credits_earned_total_bline + coll_gpa_cum_bline + terms_enrolled_bline + 
                major_pursued_chng_ever_bline + prior_stopout_ever_bline + transferee_ever_bline + 
                sw(.[,variables_list]) | factor(block), 
              cluster=~factor(block)) %>%
  etable(digits="r2")

# Get the output in a format easier to parse for tables
filter_test <- test[c(1,15:30),] %>%
  t() %>%
  as.data.frame() %>%
  mutate(across(.cols=everything(), ~na_if(., "  "))) %>%
  group_by(`Dependent Var.:`) %>%
  fill(everything(), .direction="downup") %>%
  slice_head(n=1) %>%
  select(-(starts_with("received_text_") & !ends_with("sentiment"))) 

topicvars <- c("received_text_crse_prop", "received_text_fina_prop", "received_text_plan_prop", "received_text_reso_prop",
"received_text_meet_prop")

test2 <- feols(data=regressiondata, fml=.[outcomes_list] ~ 
                age_entry + male + afam + hisp + other + race_missing +
                credits_earned_total_bline + coll_gpa_cum_bline + terms_enrolled_bline + 
                major_pursued_chng_ever_bline + prior_stopout_ever_bline + transferee_ever_bline +  
                sw(.[,topicvars]) | factor(block), 
              cluster=~factor(block)) %>%
  etable(digits="r2")

filter_test2 <- test2[c(1,15:19),] %>%
  t() %>%
  as.data.frame() %>%
  mutate(across(.cols=everything(), ~na_if(., "  "))) %>%
  group_by(`Dependent Var.:`) %>%
  fill(everything(), .direction="downup") %>%
  slice_head(n=1)

# Rewrite some of the variables for use in the table later
output_regressions <- filter_test %>% 
  left_join(filter_test2, by="Dependent Var.:") %>%
  filter(!str_detect(`Dependent Var.:`, "4")) %>%
  mutate(term=case_when(
    str_detect(`Dependent Var.:`, "1") ~ 1,
    str_detect(`Dependent Var.:`, "2") ~ 2,
    str_detect(`Dependent Var.:`, "3") ~ 3
  )) %>%
  mutate(var_order=case_when(
    str_detect(`Dependent Var.:`, "enrl") ~ 1,
    str_detect(`Dependent Var.:`, "credits") ~ 2,
    str_detect(`Dependent Var.:`, "gpa") ~ 3,
    str_detect(`Dependent Var.:`, "degree") ~ 4,
  )) %>%
  arrange(term, var_order) %>%
  select(-term, -var_order) %>%
  t() %>%
  as.data.frame()
  
write.xlsx(output_regressions, file=file.path(project_output, "regression_matrix.xlsx"), append=FALSE)

