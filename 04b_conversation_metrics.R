################################################################################
#  
#   Name: 04b_conversation_metrics.R
#   
#   Purpose: Create student-level metrics about the conversations they had
#   throughout the intervention
#
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

pacman::p_load(tidyverse, tidylog, haven, lubridate, stm, tidytext, furrr, ggthemes, ggridges, quanteda, tm)

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

## Load in the data
texts <- read_dta(file.path(project_data, '04_interactions_cleaned.dta'))%>%
  select(student_id, message_type, institution, template_id, text, time)

metadata <- read_dta(file.path(project_data, '04_students_cleaned.dta')) %>%
  select(student_id, institution, system, treatment, male, white, afam, hisp, other, race_missing, 
         risk_rating, age_entry, degree_intent_BA_bline, transferee_ever_bline)

#texts <- left_join(texts, metadata, by="student_id") %>%
#  filter(!is.na(treatment))

################################################################################
#
#   #clean - Clean and organize the dataset for text analysis
#
################################################################################

texts <- texts %>%
  mutate(template_id2 = template_id) %>%
  mutate(
    student_reply=case_when(
      message_type=="received" ~ 1,
      TRUE ~ 0
    ))
texts$template_id2[texts$template_id2==""] <- NA

texts <- texts %>% 
  arrange(student_id, time) %>%
  group_by(student_id) %>%
  fill(template_id2) %>%
  ungroup()

template_order <- texts %>%
  filter(template_id!="") %>%
  group_by(student_id, template_id) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  group_by(student_id) %>%
  mutate(scheduled_message_number = row_number(),
         scheduled_message_count = n()) %>%
  rename(scheduled_message_time = time) %>%
  select(student_id, template_id2, scheduled_message_number, scheduled_message_count, scheduled_message_time) %>%
  ungroup() %>%
  distinct()

texts <- texts %>%
  left_join(template_order, by=c("student_id", "template_id2"))

engagement <- texts %>%
  group_by(student_id, scheduled_message_number, scheduled_message_count) %>%
  filter(!is.na(scheduled_message_count)) %>%
  summarize(replied=max(student_reply)) %>%
  ungroup() %>%
  mutate(reply_number=case_when(
    replied==1 ~ as.numeric(scheduled_message_number),
    TRUE ~ as.numeric(NA)
  )) %>%
  group_by(student_id, scheduled_message_count) %>%
  summarize(scheduled_replies=sum(replied),
            scheduled_reply_proportion=sum(replied)/max(scheduled_message_count),
            first_scheduled_reply=min(reply_number, na.rm=TRUE),
            last_scheduled_reply=max(reply_number, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(first_scheduled_reply=case_when(
    first_scheduled_reply==Inf ~ as.numeric(NA),
    TRUE ~ first_scheduled_reply
  ),
  last_scheduled_reply=case_when(
    last_scheduled_reply==-Inf ~ as.numeric(NA),
    TRUE ~ last_scheduled_reply
  ),
  prop_msgs_to_engagement=first_scheduled_reply/scheduled_message_count,
  prop_msgs_to_lastengagement=last_scheduled_reply/scheduled_message_count,
  engagement_duration_prop_msgs=(last_scheduled_reply-first_scheduled_reply+1)/scheduled_message_count)

scheduled_response_times_text <- texts %>%
  filter(student_reply==1 & !is.na(template_id2)) %>%
  group_by(student_id, template_id2) %>%
  arrange(time) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  mutate(scheduled_response_time=as.numeric(dseconds(as.duration(time-scheduled_message_time)), "hours")) %>%
  filter(scheduled_response_time<336) 

scheduled_response_times <- scheduled_response_times_text %>%
  group_by(student_id) %>%
  summarize(scheduled_response_time=mean(scheduled_response_time))

scheduled_response_times_text <- scheduled_response_times_text %>%
  select(student_id, institution, template_id2, scheduled_response_time) %>%
  distinct()

all_response_times_text <- texts %>%
  group_by(student_id, template_id2) %>%
  mutate(any_responses=mean(student_reply)) %>%
  filter(any_responses>0 & !is.na(template_id2)) %>%
  mutate(initial_message_time=lag(time, n=1)) %>%
  filter(template_id=="" & message_type=="received") %>%
  mutate(all_response_time=as.numeric(dseconds(as.duration(time-initial_message_time)), "hours")) %>%
  filter(all_response_time<336) 

all_response_times <- all_response_times_text %>%
  group_by(student_id) %>%
  summarize(all_response_time=mean(all_response_time))

all_response_times_text <- all_response_times_text %>%
  group_by(student_id, template_id2, institution) %>%
  summarize(all_response_time=mean(all_response_time)) %>%
  select(student_id, institution, template_id2, all_response_time) %>%
  distinct()

scheduled_message_times <- template_order %>%
  group_by(student_id) %>%
  summarize(first_message_time = min(scheduled_message_time),
            last_message_time = max(scheduled_message_time),
            message_duration_hours= as.numeric(dseconds(as.duration(last_message_time-first_message_time+1)), "hours")) %>%
  select(student_id, first_message_time, message_duration_hours)

engagement_times <- texts %>%
  filter(message_type=="received" & !is.na(template_id2)) %>%
  group_by(student_id) %>%
  summarize(first_response_time=min(time),
            last_response_time=max(time)) %>%
  left_join(scheduled_message_times, by="student_id") %>%
  left_join(engagement, by="student_id") %>%
  mutate(
    hours_to_first_response=as.numeric(dseconds(as.duration(first_response_time-first_message_time)), "hours"),
    hours_to_last_response=as.numeric(dseconds(as.duration(last_response_time-first_message_time)), "hours"),
    engagement_duration_hours=as.numeric(dseconds(as.duration(last_response_time-first_response_time+1)), "hours"),
    prop_hours_to_engagement=hours_to_first_response/message_duration_hours,
    prop_hours_to_engagement=case_when(
      prop_hours_to_engagement>1 ~ 1,
      TRUE ~ prop_hours_to_engagement
    ),
    prop_hours_to_lastengagement=hours_to_last_response/message_duration_hours,
    prop_hours_to_lastengagement=case_when(
      prop_hours_to_lastengagement>1 ~ 1,
      TRUE ~ prop_hours_to_lastengagement
    ),
    engagement_duration_prop_hours=engagement_duration_hours/message_duration_hours,
    engagement_duration_prop_hours=case_when(
      engagement_duration_prop_hours>1 ~ 1,
      TRUE ~ engagement_duration_prop_hours
    )
  ) %>%
  select(student_id, hours_to_first_response, hours_to_last_response, engagement_duration_hours,
         prop_hours_to_engagement, prop_hours_to_lastengagement, engagement_duration_prop_hours)

help_phrases <- c("can you", "do you", "how do", "do i", "tell me", "where do", "can i",
                  "what do", "who do", "who is", "how to", "need help", "help me", "a question", "have questions",
                  "is there", "confused", "having trouble", "worried that", "not sure", "what are", "unable to",
                  "some help", "like to", "whether"
)

number_of_help_asks_text <- texts %>%
  filter(message_type=="received") %>%
  mutate(questionmarks=as.numeric(str_detect(str_to_lower(text), "\\?")),
         help_phrase=as.numeric(str_detect(str_to_lower(text), paste(help_phrases, collapse="|")))) %>%
  rowwise() %>%
  mutate(any_help=max(questionmarks, help_phrase)) %>%
  ungroup() 

number_of_help_asks <- number_of_help_asks_text %>%
  group_by(student_id) %>%
  summarize(help_ask_messages=sum(any_help),
            help_messages_prop=mean(any_help))

number_of_help_asks_text <- number_of_help_asks_text %>%
  group_by(student_id, template_id2, institution) %>%
  summarize(help_ask_messages_count=sum(any_help),
            help_messages_prop=mean(any_help),
            any_help_asks=max(any_help)) %>%
  select(student_id, institution, template_id2, help_ask_messages_count, help_messages_prop, any_help_asks) %>%
  distinct()

convosdata_text <- texts %>%
  filter(template_id=="" & !is.na(template_id2) & message_type=="received") %>% 
  group_by(student_id, template_id2) %>%
  mutate(all_text = paste(text, collapse=" ")) %>%
  add_tally() %>%
  slice(1) %>%
  ungroup() %>%
  select(student_id, template_id2, institution, n, all_text)

convosdata_text$convo_length_words <- sapply(gregexpr("[[:alpha:]]+", convosdata_text$all_text), function(x) sum(x > 0))

convosdata <- convosdata_text %>%
  select(-all_text) %>%
  group_by(student_id) %>%
  summarize(avg_convo_length_student_words=mean(convo_length_words),
            avg_convo_length_student_msgs=mean(n))

convosdata_text <- convosdata_text %>%
  select(student_id, institution, template_id2, convo_length_words, n) %>%
  rename(convo_length_student_words=convo_length_words, convo_length_student_msgs = n) %>%
  distinct()

reply_lengths_text <- texts %>%
  filter(message_type=="received" & !is.na(template_id2))

reply_lengths_text$reply_length_words <- sapply(gregexpr("[[:alpha:]]+", reply_lengths_text$text), function(x) sum(x > 0))

reply_lengths <- reply_lengths_text %>%
  mutate(substantive_reply=case_when(
    reply_length_words>=5 ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(student_id) %>%
  summarize(total_reply_length_words=sum(reply_length_words),
            avg_reply_length_words=mean(reply_length_words),
            total_reply_length_msgs=n(),
            substantive_replies=sum(substantive_reply),
            substantive_reply_prop=substantive_replies/total_reply_length_msgs)

reply_lengths_text <- reply_lengths_text %>%
  mutate(substantive_reply=case_when(
    reply_length_words>=5 ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(student_id, institution, template_id2) %>%
  summarize(substantive_reply_count=sum(substantive_reply),
            any_substantive_reply=max(substantive_reply))

output <- texts %>%
  select(student_id) %>%
  distinct() %>%
  left_join(engagement, by="student_id") %>%
  left_join(engagement_times, by="student_id") %>%
  left_join(scheduled_response_times, by="student_id") %>%
  left_join(all_response_times, by="student_id") %>%
  left_join(number_of_help_asks, by="student_id") %>%
  left_join(convosdata, by="student_id") %>%
  left_join(reply_lengths, by="student_id")

write_dta(output, file.path(project_data, paste0('04b_convosdata', filesuffix, '.dta')))

scheduled_text_level <- texts %>%
  select(student_id, institution, template_id) %>%
  distinct() %>%
  left_join(scheduled_response_times_text, by=c("student_id", "institution", "template_id"="template_id2")) %>%
  left_join(all_response_times_text, by=c("student_id", "institution", "template_id"="template_id2")) %>%
  left_join(number_of_help_asks_text, by=c("student_id", "institution", "template_id"="template_id2")) %>%
  left_join(convosdata_text, by=c("student_id", "institution", "template_id"="template_id2")) %>%
  left_join(reply_lengths_text, by=c("student_id", "institution", "template_id"="template_id2")) %>%
  rename(scheduled_response_time_hrs=scheduled_response_time, all_response_time_hrs=all_response_time) %>%
  mutate(substantive_reply_prop=substantive_reply_count/convo_length_student_msgs,
         avg_student_reply_length_words=convo_length_student_words/convo_length_student_msgs)

write_dta(scheduled_text_level, file.path(project_data, paste0('04b_scheduled_message_engagement_metrics', filesuffix, '.dta')))
