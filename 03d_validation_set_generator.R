################################################################################
#  
#   Name: 06e_validation_set_generator.R
#   
#   Purpose: Pull stratified random sample of cases from sentiment analysis
#   and topic modeling output to conduct human-algorithm validation processes
#
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

libs <- c('tidyverse', 'haven', 'lubridate', 'stm', 'tidytext', 'furrr', 'ggthemes', 'ggridges', 'quanteda', 'tm')
sapply(libs, require, character.only = TRUE)

username <- Sys.getenv("USERNAME")
home <- Sys.getenv("HOME")

setwd(home)

root <- file.path('..','Box Sync', 'N2FL NLP')
project_data <- file.path(root, 'data')
project_output <- file.path(root, 'output')

filesuffix <- "_woNA"
  # Write in a suffix to differentiate this run from others

set.seed(83191)

################################################################################
#
#   #import - Load in a dataset and start taking a peek around
#
################################################################################

## Load in the data
load(file.path(project_data, "05a_topic_model_training_temp1_woNA.RData"))
load(file.path(project_data, "05a_topic_model_training_temp2_woNA.RData"))

topic_texts <- texts_custom_merge

covariatedata <- read_dta(file.path(project_data, "04_students_cleaned.dta")) %>%
  select(student_id, system, male, white, afam) %>%
  mutate(
    race=case_when(
      white==1 ~ "White",
      afam==1 ~ "Afam",
      TRUE ~ "Other"
    ),
    race=factor(race, levels=c("White", "Afam", "Other")),
    system=factor(system, levels=c("CUNY", "TX", "VCCS"))) %>%
  select(student_id, system, race, male)

sentimentdata <- read_dta(file.path(project_data, "04_interactions_cleaned.dta"))

################################################################################
#
#   #topics - Create validation set for topic output using original convos data
#
################################################################################

getthetas2 <- function(stm_dataset) {
  temp <- stm_dataset
  outputstm <- temp %>% 
    alignCorpus(old.vocab=topic_model$vocab)
  
  counttokens <- function(x) {
    temp <- x[2,]
    output <- sum(temp)
    output
  }
  
  tokencounts_out <- sapply(outputstm$documents, counttokens)
  
  # Match up the metadata after removing empty documents in the preceding step
  newdata <- outputstm$data[!seq_len(nrow(outputstm$data)) %in% outputstm$docs.removed, ]
  
  newdocs <- fitNewDocuments(model=topic_model, documents=outputstm$documents, newData=newdata, origData=final_stm$data)
  
  outputdf <- data.frame(docnames=names(outputstm$documents), tokencounts=tokencounts_out, newdocs$theta)
  
  outputdf <- outputdf %>%
    mutate(meet_prop=X9+X15+X18+X20+X21+X22+X25+X28+X30,
           crse_prop=X10+X6+X7+X8+X12,
           fina_prop=X2+X23,
           plan_prop=X5+X13+X16+X19+X24+X26,
           reso_prop=X3+X4+X14+X17,
           gene_prop=X1+X11+X27+X29) %>%
    mutate(student_id=str_split_fixed(docnames, '_\"', 2)[,1]) %>%
    select(docnames, student_id, tokencounts, meet_prop, crse_prop, fina_prop, plan_prop, reso_prop, gene_prop)
  
  outputdf
}

topic_props <- final_stm %>%
  getthetas2()

topic_merge <- topic_texts %>%
  left_join(topic_props, by="docnames") %>%
  left_join(covariatedata, by="student_id") %>%
  filter(!is.na(meet_prop)) %>%
  rowwise() %>%
  mutate(
    highest_prop=case_when(
      meet_prop==max(c(meet_prop, crse_prop, fina_prop, plan_prop, reso_prop, gene_prop), na.rm=TRUE) ~ "meet",
      crse_prop==max(c(meet_prop, crse_prop, fina_prop, plan_prop, reso_prop, gene_prop), na.rm=TRUE) ~ "crse",
      fina_prop==max(c(meet_prop, crse_prop, fina_prop, plan_prop, reso_prop, gene_prop), na.rm=TRUE) ~ "fina",
      plan_prop==max(c(meet_prop, crse_prop, fina_prop, plan_prop, reso_prop, gene_prop), na.rm=TRUE) ~ "plan",
      reso_prop==max(c(meet_prop, crse_prop, fina_prop, plan_prop, reso_prop, gene_prop), na.rm=TRUE) ~ "reso",
      gene_prop==max(c(meet_prop, crse_prop, fina_prop, plan_prop, reso_prop, gene_prop), na.rm=TRUE) ~ "gene"
    )) %>%
  mutate(highest_prop=factor(highest_prop, levels=c("meet", "crse", "fina", "plan", "reso", "gene"))) %>%
  ungroup()
  
# Figure out how many to pull from each group; whichever groups come out of this process
# get one extra case
topic_selector <- topic_merge %>%
  filter(highest_prop!="gene") %>%
  filter(tokencounts>=10) %>%
  # This will make 90 groups...
  group_by(system, race, male, highest_prop) %>%
  summarize(count=n()) %>%
  ungroup() %>%
  mutate(extra_cases=rnorm(nrow(.))) %>%
  arrange(extra_cases) %>%
  slice_head(n=20) %>%
  mutate(keeper=1)
  
topic_samples_1 <- topic_merge %>%
  filter(highest_prop!="gene") %>%
  filter(tokencounts>=10) %>%
  mutate(picker_num=rnorm(nrow(.))) %>%
  group_by(system, race, male, highest_prop) %>%
  arrange(picker_num) %>%
  slice_head(n=2) %>%
  ungroup()

topic_samples_2 <- topic_merge %>%
  # Pick out only those that weren't part of the previous set
  anti_join(topic_samples_1, by="docnames") %>%
  filter(highest_prop!="gene") %>%
  filter(tokencounts>=10) %>%
  mutate(picker_num=rnorm(nrow(.))) %>%
  left_join(topic_selector, by=c("system", "race", "male", "highest_prop")) %>%
  filter(keeper==1) %>%
  group_by(system, race, male, highest_prop) %>%
  arrange(picker_num) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(-count, -extra_cases, -keeper) 

topic_output <- bind_rows(topic_samples_1, topic_samples_2) %>%
  rename(text=all_text) %>%
  mutate(most_prevalent="",
         second_most_prevalent="")

write_csv(topic_output, file.path(project_data, "06e_topic_validation_key.csv"))

for (i in 1:5) {
  temp <- topic_output %>%
    select(text, most_prevalent, second_most_prevalent) %>%
    mutate(random_order=rnorm(nrow(.))) %>%
    arrange(random_order) %>%
    select(-random_order)
  
  temp_a <- temp %>%
    slice_head(n=50)
  
  temp_b <- temp %>%
    slice_tail(n=150)
  
  write_csv(temp_a, file.path(project_data, paste0("06e_topic_validation_", i, "a.csv")))
  write_csv(temp_b, file.path(project_data, paste0("06e_topic_validation_", i, "b.csv")))
}

    
################################################################################
#
#   #sentiment - Get sample of sentiment output
#
################################################################################

set.seed(83191)

# Keep only one copy of each message randomly

sentiment_merge <- sentimentdata %>%
  left_join(covariatedata, by="student_id") %>%
  mutate(
    advisor_message=case_when(
      message_type=="sent" ~ 1,
      message_type=="received" ~ 0),
    custom_message=case_when(
      template_id=="" ~ 1,
      template_id!="" ~ 0)) %>%
  select(student_id, advisor_message, custom_message, lengthtest, text, system, race, male, label_predict_forest) %>%
  filter(!is.na(label_predict_forest) & !is.na(male)) %>%
  distinct(text, .keep_all=TRUE)
  
sentiment_selector <- sentiment_merge %>%
  filter(lengthtest>=100 & custom_message==1) %>%
  # This will make 90 groups...
  group_by(system, race, male, advisor_message) %>%
  summarize(count=n()) %>%
  ungroup() 

sentiment_samples_1 <- sentiment_merge %>%
  filter(lengthtest>=100 & custom_message==1) %>%
  mutate(picker_num=rnorm(nrow(.))) %>%
  group_by(system, race, male, advisor_message) %>%
  arrange(picker_num) %>%
  slice_head(n=5) %>%
  ungroup()

sentiment_samples_2 <- sentiment_merge %>%
  filter(lengthtest>=100 & custom_message==1) %>%
  anti_join(sentiment_samples_1, by="text") %>%
  mutate(picker_num=rnorm(nrow(.))) %>%
  arrange(picker_num) %>%
  slice_head(n=20) %>%
  ungroup()

sentiment_output <- bind_rows(sentiment_samples_1, sentiment_samples_2) %>%
  mutate(sentiment_score="")

write_csv(sentiment_output, file.path(project_data, "06e_sentiment_validation_key.csv"))

for (i in 1:5) {
  temp <- sentiment_output %>%
    select(text, sentiment_score) %>%
    mutate(random_order=rnorm(nrow(.))) %>%
    arrange(random_order) %>%
    select(-random_order)
  
  temp_a <- temp %>%
    slice_head(n=50)
  
  temp_b <- temp %>%
    slice_tail(n=150)
  
  write_csv(temp_a, file.path(project_data, paste0("06e_sentiment_validation_", i, "a.csv")))
  write_csv(temp_b, file.path(project_data, paste0("06e_sentiment_validation_", i, "b.csv")))
}


