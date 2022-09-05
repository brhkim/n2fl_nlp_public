################################################################################
#  
#   Name: 06g_validation_set_algocompare.R
#   
#   Purpose: Check each of the sentiment analysis algorithm's output on
#   the validation set against human coders. Then identify which algorithm
#   produces the highest accuracy output across subgroups.
#
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

libs <- c('tidyverse', 'readxl', 'haven', 'lubridate', 'ggthemes', 'ggridges', 
          'tidylog', 'caret', 'irr', 'stringr', 'kableExtra')
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

test <- read_csv(file.path(project_data, "03c_forest_output_masked.csv")) %>%
  mutate(
    label_predict_forest = case_when(
      label_predict_forest=="LABEL_0" ~ -2,
      label_predict_forest=="LABEL_1" ~ -1,
      label_predict_forest=="LABEL_2" ~ 0,
      label_predict_forest=="LABEL_3" ~ 1,
      label_predict_forest=="LABEL_4" ~ 2,
    ),
    label_predict_yelp = case_when(
      yelp__LABEL_0==1 ~ -2,
      yelp__LABEL_1==1 ~ -1,
      yelp__LABEL_2==1 ~ 0,
      yelp__LABEL_3==1 ~ 1,
      yelp__LABEL_4==1 ~ 2
    ),
    label_predict_xlnet = case_when(
      xlnet__LABEL_0==1 ~ -1,
      xlnet__LABEL_4==1 ~ 1
    ),
    label_predict_albert = case_when(
      albert__LABEL_0==1 ~ -1,
      albert__LABEL_4==1 ~ 1
    ),
    label_predict_stanza = case_when(
      stanza__LABEL_0==1 ~ -1,
      stanza__LABEL_2==1 ~ 0,
      stanza__LABEL_4==1 ~ 1
    ),
    label_predict_bert = case_when(
      bert__LABEL_0==1 ~ -2,
      bert__LABEL_1==1 ~ -1,
      bert__LABEL_2==1 ~ 0,
      bert__LABEL_3==1 ~ 1,
      bert__LABEL_4==1 ~ 2
    ),
    label_predict_twit = case_when(
      twit__LABEL_0==1 ~ -1,
      twit__LABEL_2==1 ~ 0,
      twit__LABEL_4==1 ~ 1
    ),
    label_predict_imdb = case_when(
      imdb__LABEL_0==1 ~ -1,
      imdb__LABEL_2==1 ~ 0,
      imdb__LABEL_4==1 ~ 1
    )
  ) %>%
  select(-contains("__")) %>%
  select(text, label_predict_forest, label_predict_yelp, label_predict_xlnet,
         label_predict_albert, label_predict_stanza, label_predict_bert, label_predict_twit,
         label_predict_imdb) %>%
  distinct()


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
  select(-sentiment_score, -label_predict_forest)

test <- sentiment_key %>%
  left_join(sentiment_BK, by="text") %>%
  left_join(sentiment_DP, by="text") %>%
  left_join(sentiment_EA, by="text") %>%
  left_join(sentiment_KM, by="text") %>%
  left_join(sentiment_AC, by="text") %>%
  left_join(test, by="text") 


# 5 level: Forest, Yelp, bert
# 2 level: xlnet, albert
# 3 level: stanza, twit


algorithms_5 <- c("label_predict_forest", "label_predict_yelp", "label_predict_bert")

matrix_output_5 <- matrix(, nrow=length(initials), ncol=length(algorithms_5))
rownames(matrix_output_5) <- initials
colnames(matrix_output_5) <- algorithms_5

for(i in 1:length(algorithms_5)) {
  for(j in 1:length(initials)) {
    algorithm <- algorithms_5[i]
    initial <- initials[j]
    
    coder_var <- paste0("score_", initial)
    
    combined <- bind_cols(algo_scores=test[[algorithm]], coder_scores=test[[coder_var]]) %>%
      mutate(correct=case_when(
        algo_scores==coder_scores ~ 1,
        TRUE ~ 0
      ))
    
    output_accuracy <- combined$correct %>%
      mean()
    
    matrix_output_5[j, i] <- output_accuracy
  }
}

write_csv(matrix_output_5 %>% as.data.frame() %>% mutate(coder=rownames(matrix_output_5)), file="matrix_output_5_perfect.csv")

disparate_accuracy_check <- function(df, algorithmname) {
  df <- df %>%
    as.data.frame()
  
  matrix_output_5 <- matrix(, nrow=length(initials), ncol=1)
  rownames(matrix_output_5) <- initials
  
  for(j in 1:length(initials)) {
    initial <- initials[j]
    
    coder_var <- paste0("score_", initial)
    
    combined <- bind_cols(algo_scores=df[[algorithmname]], coder_scores=df[[coder_var]]) %>%
      mutate(correct=case_when(
        algo_scores==coder_scores ~ 1,
        TRUE ~ 0
      ))
    
    output_accuracy <- combined$correct %>%
      mean()
    
    matrix_output_5[j, 1] <- output_accuracy
  }
  
  matrix_output_5 %>% mean()
}


female_0 <- test %>%
  filter(male==1)

female_1 <- test %>%
  filter(male==0)

white_1 <- test %>%
  filter(race=="White")

black_1 <- test %>%
  filter(race=="Afam")

other_1 <- test %>%
  filter(race=="Other")

system_cuny <- test %>%
  filter(system=="CUNY")

system_tx <- test %>%
  filter(system=="TX")

system_vccs <- test %>%
  filter(system=="VCCS")

advisor_0 <- test %>%
  filter(advisor_message==0)

advisor_1 <- test %>%
  filter(advisor_message==1)

datachecks <- list(test, female_0, female_1, white_1, black_1, other_1, 
                   system_cuny, system_tx, system_vccs,
                   advisor_0, advisor_1)

labels <- c("Overall", "Female", "Male", "White", "Black", "Other", "CUNY", "TX", "VCCS", "Advisor", "Student")
algorithm_labels <- c("Random Forest", "BERT Yelp", "BERT Products")

r <- 1
output <- matrix(data=NA, nrow=length(labels), ncol=length(algorithms_5))

for(dataframe in datachecks) {
  c <- 1
  
  for(algorithm in algorithms_5) {
    tmp <- disparate_accuracy_check(df=dataframe, algorithmname=algorithm)
    
    output[r, c] <- round(tmp, 3)
    
    c <- c + 1
  }
  
  r <- r + 1
}

observations <- c(nrow(test), nrow(female_0), nrow(female_1),
                  nrow(white_1), nrow(black_1), nrow(other_1),
                  nrow(system_cuny), nrow(system_tx), nrow(system_vccs),
                  nrow(advisor_0), nrow(advisor_1))

colnames(output) <- algorithm_labels
output <- output %>%
  as.data.frame() %>%
  mutate(Subset=labels, `Obs.`=observations) %>%
  relocate(Subset, `Obs.`)

kable(output, align = c("r", "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2, width="6em", border_right=T) %>%
  column_spec(1:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(3, extra_css="border-bottom: 1px solid") %>%
  row_spec(6, extra_css="border-bottom: 1px solid") %>%
  row_spec(9, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(paste0("06g_algorithm_comparison.jpg"), zoom=3, bs_theme="default")


# Analyze one-off accuracy
algorithms_5 <- c("label_predict_forest", "label_predict_yelp", "label_predict_bert")

matrix_output_5 <- matrix(, nrow=length(initials), ncol=length(algorithms_5))
rownames(matrix_output_5) <- initials
colnames(matrix_output_5) <- algorithms_5

for(i in 1:length(algorithms_5)) {
  for(j in 1:length(initials)) {
    algorithm <- algorithms_5[i]
    initial <- initials[j]
    
    coder_var <- paste0("score_", initial)
    
    combined <- bind_cols(algo_scores=test[[algorithm]], coder_scores=test[[coder_var]]) %>%
      mutate(correct=case_when(
        #algo_scores==coder_scores ~ 1,
        algo_scores==coder_scores | algo_scores==coder_scores-1 | algo_scores==coder_scores+1 ~ 1,
        TRUE ~ 0
      ))
    
    output_accuracy <- combined$correct %>%
      mean()
    
    matrix_output_5[j, i] <- output_accuracy
  }
}

write_csv(matrix_output_5 %>% as.data.frame() %>% mutate(coder=rownames(matrix_output_5)), file="matrix_output_5_perfect.csv")



algorithms_3 <- c("label_predict_forest", "label_predict_yelp", "label_predict_bert", "label_predict_stanza", "label_predict_twit")

matrix_output_3 <- matrix(, nrow=length(initials), ncol=length(algorithms_3))
rownames(matrix_output_3) <- initials
colnames(matrix_output_3) <- algorithms_3

for(i in 1:length(algorithms_3)) {
  for(j in 1:length(initials)) {
    algorithm <- algorithms_3[i]
    initial <- initials[j]
    
    coder_var <- paste0("score_", initial)
    
    combined <- bind_cols(algo_scores=test[[algorithm]], coder_scores=test[[coder_var]]) %>%
      mutate(algo_scores=case_when(
        algo_scores==2 ~ 1,
        algo_scores==-2 ~ -1,
        TRUE ~ algo_scores
      )) %>%
      mutate(coder_scores=case_when(
        coder_scores==2 ~ 1,
        coder_scores==-2 ~ -1,
        TRUE ~ coder_scores
      )) %>%
      mutate(correct=case_when(
        algo_scores==coder_scores ~ 1,
        TRUE ~ 0
      ))
    
    output_accuracy <- combined$correct %>%
      mean()
    
    matrix_output_3[j, i] <- output_accuracy
  }
}

write_csv(matrix_output_3 %>% as.data.frame() %>% mutate(coder=rownames(matrix_output_3)), file="matrix_output_3.csv")


test<-matrix_output_3 %>% as.data.frame()

