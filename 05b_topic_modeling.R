################################################################################
#  
#   Name: 05b_topic_modeling.R
#   
#   Purpose: Conduct topic modeling on the text data from N2FL texting intervention
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

runchecker <- FALSE 
  # Set to true if wanting to run a new instance for hand-spellchecking/replacing words

filesuffix <- "_woNA"
  # Write in a suffix to differentiate this run from others

################################################################################
#
#   #import - Load in a dataset and start taking a peek around
#
################################################################################

## Load in other topic modeling data
base::load(file=file.path(project_data, paste0('05a_topic_model_training_temp1', filesuffix, '.RData')))
base::load(file=file.path(project_data, paste0('05a_topic_model_training_temp2', filesuffix, '.RData')))

## Load in the raw data
texts <- read_dta(file.path(project_data, '04_interactions_cleaned.dta')) %>%
  select(student_id, template_id, message_type, text, time) 

metadata <- read_dta(file.path(project_data, '04_students_cleaned.dta')) %>%
  select(student_id, institution, system, treatment, male, white, afam, hisp, other, race_missing, 
         risk_rating, age_entry, degree_intent_BA_bline, transferee_ever_bline)

texts <- left_join(texts, metadata, by="student_id") %>%
  filter(!is.na(treatment))



################################################################################
#
#   #clean - Clean and organize the dataset for topic modeling
#
################################################################################

# Properly label each type of text
texts <- texts %>%
  rename(all_text=text) %>%
  group_by(student_id) %>%
  mutate(message_id=paste0(student_id, "_", row_number())) %>%
  ungroup() %>%
  mutate(g_all_text=1,
         g_form_texts=case_when(template_id=="" & message_type=="sent"  ~ 0,
                                template_id!="" & message_type=="sent"  ~ 1,
                                TRUE ~ 0),
         g_nonform_texts=case_when(template_id=="" & message_type=="sent" ~ 1,
                                  template_id!="" & message_type=="sent"  ~ 0,
                                  TRUE ~ 0),
         g_sent_text=case_when(message_type=="sent" ~ 1,
                              message_type=="received"  ~ 0),
         g_received_text=case_when(message_type=="sent" ~ 0,
                                   message_type=="received"  ~ 1))

texttypes <- texts %>%
  select(message_id, student_id, starts_with("g_"))

texts_custom_msg <- texts %>%
  # Replace email addresses
  mutate(all_text=str_replace_all(all_text, "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "tokenemailaddress")) %>%
  # Replace URLs
  mutate(all_text=str_replace_all(all_text, "(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)", "tokenurl")) %>%
  # Replace phone numbers
  mutate(all_text=str_replace_all(all_text, "([\\(]?[0-9]{3}[-\\s\\(\\)])?[0-9]{3}[-\\s\\(\\)][0-9]{4}", "tokenphonenumber"))

# texts_custom_merge <- texts_custom %>%
#   select(student_id, all_text) %>%
#   rename(docnames=student_id)

# Tokenize and remove stop-words
tidy_texts_custom_msg <- texts_custom_msg %>%
  unnest_tokens(input=all_text, output=word, token="tweets", drop=TRUE) %>%
  anti_join(stop_words)

# Remove numbers more generally
nums <- tidy_texts_custom_msg %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
tidy_texts_custom_msg <- tidy_texts_custom_msg %>%
  anti_join(nums)

# Replace school and system names
schoolnames <- metadata %>%
  select(institution) %>%
  unique() %>%
  rename(word=institution) %>%
  mutate(word=str_to_lower(word))

systemnames <- metadata %>%
  select(system) %>%
  unique() %>%
  rename(word=system) %>%
  filter(word!="") %>%
  mutate(word=str_to_lower(word))

othernames <- c("laguardia", "ny", "john", "jay", "austin", "kingsborough", "permian", "basin", "arlington", "texas", "jj" ,"kcc", "utsa", "leh", "tamu", "ut")
othernames <- data.frame(word=othernames)

joinednames <- bind_rows(schoolnames, systemnames, othernames)

tidy_texts_custom_msg <- tidy_texts_custom_msg %>%
  mutate(word=case_when(word %in% joinednames$word ~ "tokensystemname",
                        TRUE ~ word))

if (runchecker==TRUE){
  checker <- tidy_texts_custom %>%
    count(word, sort = TRUE) %>%
    head(n=2000)
  
  write_csv(checker, file.path(project_data, paste0('05a_wordlist', filesuffix, '.csv')))
}

# Use hand-generated replacement list
replacements <- read_csv(file.path(project_data, '05a_wordlist_edited.csv')) %>%
  select(-n) %>%
  filter(!is.na(replacement))

for(i in 1:nrow(replacements)) {
  stringtext <- replacements$word[i]
  replacementtext <- replacements$replacement[i]
  tidy_texts_custom_msg <- tidy_texts_custom_msg %>%
    mutate(word=case_when(word==stringtext ~ replacementtext,
                          TRUE ~ word))
}

custom_dfm_msg <- tidy_texts_custom_msg %>%
  count(message_id, word, sort = TRUE) %>%
  cast_dfm(document=message_id, term=word, value=n) %>%
  dfm_subset(ntoken(.)>0)

retokenized_msg <- tidy_texts_custom_msg %>%
  select(message_id, word) %>%
  group_by(message_id) %>% 
  mutate(all_text=paste(word, collapse=" ")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-word)

tidy_texts_custom_msg2 <- retokenized_msg %>%
  unnest_tokens(input=all_text, output=word, token="ngrams", n=2, drop=TRUE)

custom_dfm_msg2 <- tidy_texts_custom_msg2 %>%
  count(message_id, word, sort = TRUE) %>%
  filter(!is.na(word)) %>% # Remove this to reintroduce NAs
  cast_dfm(document=message_id, term=word, value=n) %>%
  dfm_subset(ntoken(.)>0)

missingdata <- metadata %>%
  filter(is.na(treatment))

final_dfm_msg <- rbind(custom_dfm_msg, custom_dfm_msg2) %>% 
  dfm_compress(margin="documents") %>%
  # Manually removing erroneous entry with no metadata
  dfm_subset(!str_split_fixed(docnames(.), '_\"', 2)[,1] %in% missingdata$student_id) %>%
  dfm_subset(str_split_fixed(docnames(.), '_\"', 2)[,1]!="314649")

final_dfm_msg <- dfm_match(final_dfm_msg, featnames(final_dfm)) %>%
  dfm_subset(ntoken(.)>0)

docvarmerge <- left_join(data.frame(message_id=docnames(final_dfm_msg)), texttypes, by="message_id")

docvars(final_dfm_msg) <- docvarmerge

getthetas <- function(dfm) {
  temp <- dfm %>%
    dfm_group(groups=student_id) %>%
    dfm_subset(ntoken(.)>0)
    
  tempmetaprep <- data.frame(docnames=docnames(temp))
  
  tempmetaprep2 <- left_join(tempmetaprep, metadata, by=c("docnames" = "student_id")) %>%
    mutate(treatment=treatment-1,
           over_23=case_when(age_entry >= 24 ~ 1,
                             TRUE ~ 0)) 
  
  outputstm <- asSTMCorpus(temp, data=tempmetaprep2) %>% 
    alignCorpus(old.vocab=topic_model$vocab)
  
  # Match up the metadata after removing empty documents in the preceding step
  newdata <- outputstm$data[!seq_len(nrow(outputstm$data)) %in% outputstm$docs.removed, ]
  
  newdocs <- fitNewDocuments(model=topic_model, documents=outputstm$documents, newData=newdata, origData=final_stm$data)
  
  outputdf <- data.frame(student_id=names(outputstm$documents), newdocs$theta)
  
  outputdf <- outputdf %>%
    mutate(meet_prop=X9+X15+X18+X20+X21+X22+X25+X28+X30,
           crse_prop=X10+X6+X7+X8+X12,
           fina_prop=X2+X23,
           plan_prop=X5+X13+X16+X19+X24+X26,
           reso_prop=X3+X4+X14+X17,
           gene_prop=X1+X11+X27+X29) %>%
    select(student_id, meet_prop, crse_prop, fina_prop, plan_prop, reso_prop, gene_prop)
  
  outputdf
}

final_stm_all_text <- final_dfm_msg %>%
  dfm_subset(g_all_text==1) %>%
  getthetas() %>%
  mutate(group="all_text") %>%
  rename_at(vars(ends_with("_prop")), ~paste0("all_text_", .))

final_stm_form <- final_dfm_msg %>%
  dfm_subset(g_form_texts==1) %>%
  getthetas() %>%
  mutate(group="form_text") %>%
  rename_at(vars(ends_with("_prop")), ~paste0("form_text_", .))

final_stm_nonform <- final_dfm_msg %>%
  dfm_subset(g_nonform_texts==1) %>%
  getthetas() %>%
  mutate(group="nonform_text") %>%
  rename_at(vars(ends_with("_prop")), ~paste0("nonform_text_", .))

final_stm_sent <- final_dfm_msg %>%
  dfm_subset(g_sent_text==1) %>%
  getthetas() %>%
  mutate(group="sent_text") %>%
  rename_at(vars(ends_with("_prop")), ~paste0("sent_text_", .))

final_stm_received <- final_dfm_msg %>%
  dfm_subset(g_received_text==1) %>%
  getthetas() %>%
  mutate(group="received_text") %>%
  rename_at(vars(ends_with("_prop")), ~paste0("received_text_", .))

output <- left_join(final_stm_all_text, final_stm_form, by="student_id") %>%
  left_join(final_stm_nonform, by="student_id") %>%
  left_join(final_stm_sent, by="student_id") %>%
  left_join(final_stm_received, by="student_id") %>%
  select(-starts_with("group"))

write_dta(output, file.path(project_data, paste0('05b_processed_topic_data.dta')))

