################################################################################
#  
#   Name: 06f_validation_set_analysis.R
#   
#   Purpose: Calculate IRR values for sentiment analysis and topic modeling
#   using human-generated ratings against algorithmic output.
#
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

libs <- c('tidyverse', 'readxl', 'haven', 'lubridate', 'ggthemes', 'ggridges', 'tidylog', 'caret', 'irr', 'stringr')
sapply(libs, require, character.only = TRUE)

username <- Sys.getenv("USERNAME")
home <- Sys.getenv("HOME")

setwd("C:/Users/brhki/My Drive/NLP Validation Test")

validation_data <- "C:/Users/brhki/My Drive/NLP Validation Test/completed_templates"
project_data <- "C:/Users/brhki/My Drive/NLP Validation Test"
project_output <- "C:/Users/brhki/My Drive/NLP Validation Test"

filesuffix <- "_woNA"
  # Write in a suffix to differentiate this run from others

set.seed(83191)

################################################################################
#
#   #import - Load in a datasets and start taking a peek around
#
################################################################################

initials <- c("BK", "DP", "EA", "KM", "AC")

# First deal with sentiment analysis datasets
oldnames <- c("text", "sentiment_score")

for (i in initials) {
  newnames <- c("text", paste0("score_", i))
  
  tmp1 <- read_csv(file.path(validation_data, paste0("06e_sentiment_validation_", i, "a.csv"))) %>%
    select(text, sentiment_score) %>%
    rename_with(~ newnames, all_of(oldnames))
  
  tmp2 <- read_csv(file.path(validation_data, paste0("06e_sentiment_validation_", i, "b.csv"))) %>%
    select(text, sentiment_score) %>%
    rename_with(~ newnames, all_of(oldnames))
  
  output <- tmp1 %>%
    bind_rows(tmp2)
  
  assign(value=output, x=paste0("sentiment_", i))
}

sentiment_key <- read_csv(file.path(project_data, "06e_sentiment_validation_key.csv")) %>%
  select(-sentiment_score)


# Now deal with topic modeling
oldnames <- c("text", "most_prevalent", "second_most_prevalent")

for (i in initials) {
  newnames <- c("text", paste0(i, "1"), paste0(i, "2"))
  
  tmp1 <- read_xlsx(file.path(validation_data, paste0("06e_topic_validation_", i, "a.xlsx"))) %>%
    select(text, most_prevalent, second_most_prevalent) %>%
    rename_with(~ newnames, all_of(oldnames))
  
  tmp2 <- read_xlsx(file.path(validation_data, paste0("06e_topic_validation_", i, "b.xlsx"))) %>%
    select(text, most_prevalent, second_most_prevalent) %>%
    rename_with(~ newnames, all_of(oldnames))
  
  output <- tmp1 %>%
    bind_rows(tmp2)
  
  assign(value=output, x=paste0("topic_", i))
}

topic_key <- read_csv(file.path(project_data, "06e_topic_validation_key.csv"),  locale = readr::locale(encoding = "windows-1252")) %>%
  mutate(
    highest_prop=case_when(
      highest_prop=="crse" ~ "Course Selection and Enrollment",
      highest_prop=="fina" ~ "Finances",
      highest_prop=="meet" ~ "Advising Logistics",
      highest_prop=="plan" ~ "Academic Planning",
      highest_prop=="reso" ~ "Academic Resources"
    ))

################################################################################
#
#   #merge - Merge datasets together and begin analysis
#
################################################################################


# Sentiment was stratified by: system, race, male, advisor_message (all custom only; 100 chars or more)

# Topic was stratified by: "system", "race", "male", "highest_prop" (only 10 tokens or more)

sentiment_merged <- sentiment_key %>%
  left_join(sentiment_BK, by="text") %>%
  left_join(sentiment_DP, by="text") %>%
  left_join(sentiment_EA, by="text") %>%
  left_join(sentiment_KM, by="text") %>%
  left_join(sentiment_AC, by="text") %>%
  mutate(error_BK=abs(label_predict_forest-score_BK),
         error_DP=abs(label_predict_forest-score_DP),
         error_EA=abs(label_predict_forest-score_EA),
         error_KM=abs(label_predict_forest-score_KM),
         error_AC=abs(label_predict_forest-score_AC),
         sum_error=error_BK+error_DP+error_EA+error_KM+error_AC)
# 
# confusionMatrix(data=test$label_predict_forest, reference=test$score_BK)
# confusionMatrix(data=test$label_predict_forest, reference=test$score_DP)
# confusionMatrix(data=test$label_predict_forest, reference=test$score_EA)
# confusionMatrix(data=test$label_predict_forest, reference=test$score_KM)
# confusionMatrix(data=test$label_predict_forest, reference=test$score_AC)
# 
# test %>% count(error_BK)
# test %>% count(error_DP)
# test %>% count(error_EA)
# test %>% count(error_KM)
# test %>% count(error_AC)


icclist_to_df <- function(icclist) {
  output <- as.data.frame(unlist(icclist, use.names=FALSE))
  
  output <- unlist(icclist, use.names=FALSE)
  
  tmp <- data.frame(vars=names(icclist), vals=output) %>%
    pivot_wider(names_from="vars", values_from="vals")
}

relcomp <- function(a, b) {
  comp <- vector()
  
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

ICC_check <- function(df) {
  
  df_without <- df %>%
    select(starts_with("score_"))
  
  icc_without <- df_without %>%
    icc(ratings=., model="twoway", type="agreement", unit="single")  %>%
    icclist_to_df() %>%
    mutate(icc_type="without")
  
  icc_with <- df %>%
    select(starts_with("score_"), label_predict_forest) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single")  %>%
    icclist_to_df() %>%
    mutate(icc_type="with")
  
  # Hypothetical "worst case scenario"
  options <- c(-2, -1, 0, 1, 2)
  
  df_adversarial <- df_without
  df_adversarial$adversarial <- NA
  
  set.seed(1234)
  for (i in 1:length(df_adversarial$score_BK)) {
    uniquecheck <- c(df_adversarial$score_BK[i], df_adversarial$score_DP[i], df_adversarial$score_EA[i], df_adversarial$score_KM[i]) %>%
      unique()
    
    adversarial_options <- relcomp(options, uniquecheck)
    
    df_adversarial$adversarial[i] <- sample(adversarial_options, 1)
  }
  
  icc_adversarial <- df_adversarial %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="adversarial")
  
  
  # Hypothetical uninformed guessing
  set.seed(1234)
  random <- sample(-2:2, size=nrow(df), replace=TRUE)
  
  icc_random <- df_without %>%
    bind_cols(random) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="random")
  
  # All one answer
  icc_vneg <- df_without %>%
    mutate(add=-2) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="vneg")
  
  icc_neg <- df_without %>%
    mutate(add=-1) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="neg")
  
  icc_neu <- df_without %>%
    mutate(add=0) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="neu")
  
  icc_pos <- df_without %>%
    mutate(add=1) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="pos")
  
  icc_vpos <- df_without %>%
    mutate(add=2) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="vpos")
  
  output <- bind_rows(icc_without, icc_with, icc_adversarial, icc_random, icc_vneg, icc_neg, icc_neu, icc_pos, icc_vpos)
}

sent_all <- sentiment_merged %>%
  ICC_check()

sent_tx <- sentiment_merged %>%
  filter(system=="TX") %>%
  ICC_check() %>%
  mutate(system="TX")

sent_vccs <- sentiment_merged %>%
  filter(system=="VCCS") %>%
  ICC_check() %>%
  mutate(system="TX")

sent_cuny <- sentiment_merged %>%
  filter(system=="CUNY") %>%
  ICC_check() %>%
  mutate(system="TX")

sent_sys <- bind_rows(sent_tx, sent_vccs, sent_cuny)

sent_male <- sentiment_merged %>%
  filter(male==1) %>%
  ICC_check() %>%
  mutate(male="1")

sent_female <- sentiment_merged %>%
  filter(male==0) %>%
  ICC_check() %>%
  mutate(male="0")

sent_gender <- bind_rows(sent_male, sent_female)

sent_afam <- sentiment_merged %>%
  filter(race=="Afam") %>%
  ICC_check() %>%
  mutate(race="Afam")

sent_white <- sentiment_merged %>%
  filter(race=="White") %>%
  ICC_check() %>%
  mutate(race="White")

sent_other <- sentiment_merged %>%
  filter(race=="Other") %>%
  ICC_check() %>%
  mutate(race="Other")

sent_race <- bind_rows(sent_afam, sent_white, sent_other)

sent_advisor <- sentiment_merged %>%
  filter(advisor_message==1) %>%
  ICC_check() %>%
  mutate(advisor="1")

sent_student <- sentiment_merged %>%
  filter(advisor_message==0) %>%
  ICC_check() %>%
  mutate(advisor="0")

sent_type <- bind_rows(sent_advisor, sent_student)
# Sentiment was stratified by: system, race, male, advisor_message (all custom only; 100 chars or more)

ggplot(sent_all %>% 
         filter(icc_type %in% c("with", "without", "random", "adversarial")) %>%
         mutate(icc_type=case_when(
           icc_type=="with" ~ "With Algorithm",
           icc_type=="without" ~ "Without Algorithm",
           icc_type=="random" ~ "Random Distributional Guessing",
           icc_type=="adversarial" ~ "Adversarial Guessing"
         ),
         icc_type=factor(icc_type, levels=c("Without Algorithm", "With Algorithm",
                                            "Random Distributional Guessing", 
                                            "Adversarial Guessing")))
       , aes(x=icc_type, y=as.numeric(value))) + 
  geom_pointrange(aes(ymin=as.numeric(lbound), ymax=as.numeric(ubound))) +
  scale_x_discrete(labels=scales::wrap_format(20)) + 
  labs(
    y="Intraclass Correlation Coefficient \n ",
    x=" \n Rater Group",
  ) + 
  theme_bw()

ggsave(file="06f_icc_metrics.png", width=6, height=4)




topic_merged <- topic_key %>%
  left_join(topic_BK, by="text") %>%
  left_join(topic_DP, by="text") %>%
  left_join(topic_EA, by="text") %>%
  left_join(topic_KM, by="text") %>%
  left_join(topic_AC, by="text")

test <- topic_merged %>%
  mutate(
    error_bk=case_when(
      highest_prop==BK1 ~ "Perfect Match",
      highest_prop==BK2 ~ "Secondary Match",
      !is.na(BK1) & highest_prop!=BK1 & highest_prop!=BK2 ~ "No Match",
      is.na(BK1) ~ as.character(NA)
    ),
    error_dp=case_when(
      highest_prop==DP1 ~ "Perfect Match",
      highest_prop==DP2 ~ "Secondary Match",
      !is.na(DP1) & highest_prop!=DP1 & highest_prop!=DP2 ~ "No Match",
      is.na(DP1) ~ as.character(NA)
    ),
    error_ea=case_when(
      highest_prop==EA1 ~ "Perfect Match",
      highest_prop==EA2 ~ "Secondary Match",
      !is.na(EA1) & highest_prop!=EA1 & highest_prop!=EA2 ~ "No Match",
      is.na(EA1) ~ as.character(NA)
    ),
    error_km=case_when(
      highest_prop==KM1 ~ "Perfect Match",
      highest_prop==KM2 ~ "Secondary Match",
      !is.na(KM1) & highest_prop!=KM1 & highest_prop!=KM2 ~ "No Match",
      is.na(KM1) ~ as.character(NA)
    ),
    error_ac=case_when(
      highest_prop==AC1 ~ "Perfect Match",
      highest_prop==AC2 ~ "Secondary Match",
      !is.na(AC1) & highest_prop!=AC1 & highest_prop!=AC2 ~ "No Match",
      is.na(AC1) ~ as.character(NA)
    )
  ) %>%
  filter(AC1!="")

# test %>% count(error_bk)
# test %>% count(error_dp)
# test %>% count(error_ea)
# test %>% count(error_km)
# test %>% count(error_ac)

relcomp <- function(a, b) {
  comp <- vector()
  
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

kappam_to_df <- function(kappam) {
  output <- as.data.frame(unlist(kappam, use.names=FALSE))
  
  output <- unlist(kappam, use.names=FALSE)
  
  tmp <- data.frame(vars=names(kappam), vals=output) %>%
    pivot_wider(names_from="vars", values_from="vals")
}

kappam_check <- function(df) {
  
  df <- test
  
  df_without <- df %>%
    select(BK1, AC1, KM1, EA1, DP1) %>%
    mutate(across(everything(), ~ factor(., levels=c("Academic Planning", "Academic Resources", "Advising Logistics",
                                                     "Course Selection and Enrollment", "Finances", "General Conversation or Other"))))
  
  topic_without <- df_without %>%
    kappam.light() %>%
    kappam_to_df() %>%
    mutate(kappam_type="without")
  
  topic_with <- df %>%
    select(BK1, AC1, KM1, EA1, DP1, highest_prop) %>%
    mutate(across(everything(), ~ factor(., levels=c("Academic Planning", "Academic Resources", "Advising Logistics",
                                                     "Course Selection and Enrollment", "Finances", "General Conversation or Other")))) %>%
    kappam.light() %>%
    kappam_to_df() %>%
    mutate(kappam_type="with")
  
  df_adversarial <- df_without %>%
    mutate(across(everything(), as.numeric))
  
  options <- c(1, 2, 3, 4, 5, 6)
  
  df_adversarial$adversarial <- NA
  
  set.seed(1234)
  
  for (i in 1:length(df_adversarial$BK1)) {
    uniquecheck <- c(df_adversarial$BK1[i], df_adversarial$DP1[i], df_adversarial$EA1[i], df_adversarial$KM1[i]) %>%
      unique()
    
    adversarial_options <- relcomp(options, uniquecheck)
    
    df_adversarial$adversarial[i] <- sample(adversarial_options, 1)
  }
  
  topic_adversarial <- df_adversarial %>%
    kappam.light() %>%
    kappam_to_df() %>%
    mutate(kappam_type="adversarial")
  
  # Hypothetical uninformed guessing
  set.seed(1234)
  random <- sample(1:6, size=nrow(df), replace=TRUE)
  
  topic_random <- df_without %>%
    bind_cols(random) %>%
    kappam.light() %>%
    kappam_to_df() %>%
    mutate(kappam_type="random")
  
  prevalence <- df_without %>%
    pivot_longer(cols=everything())
  
  set.seed(1234)
  
  prevalence2 <- sample(prevalence$value, size=nrow(df))
  
  df_prev <- df_without
  df_prev$prev <- prevalence2
  
  topic_prev <- df_prev %>%
    kappam.light() %>%
    kappam_to_df() %>%
    mutate(kappam_type="prevalence")
  
  
  df_ideal <- df_without %>%
    mutate(across(everything(), as.numeric))
  
  df_ideal$ideal <- NA
  
  set.seed(1234)
  
  for (i in 1:length(df_ideal$BK1)) {
    data <- df_ideal %>%
      slice(i) %>%
      select(-ideal) %>%
      pivot_longer(cols=everything()) %>%
      count(value) %>%
      arrange(-n) %>%
      slice_head() %>%
      slice_sample(n=1)
    
    df_ideal$ideal[i] <- data$value[1]
  }
  
  topic_ideal <- df_ideal %>%
    kappam.light() %>%
    kappam_to_df() %>%
    mutate(kappam_type="ideal")
  
  output <- bind_rows(topic_without, topic_with, topic_adversarial, topic_random, topic_prev, topic_ideal)
  
  output
  
}

topic_test <- test %>%
  kappam_check()



