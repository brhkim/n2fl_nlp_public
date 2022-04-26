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


################################################################################
#
#   #clean - Clean and organize the dataset for text analysis
#
################################################################################

# First make an ID for messages at the "conversation" level based on the scheduled message that precedes it
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

# Also get a separate dataset to keep track of the order of scheduled messages for each particular student
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

# Merge the order data back in for convenience
texts <- texts %>%
  left_join(template_order, by=c("student_id", "template_id2"))

# Now make measures for basic engagement
engagement <- texts %>%
  group_by(student_id, scheduled_message_number, scheduled_message_count) %>%
  filter(!is.na(scheduled_message_count)) %>%
  # Check whether a student replied at all in a given conversation
  summarize(replied=max(student_reply)) %>%
  ungroup() %>%
  # Get a count of the reply number if more than 1
  mutate(reply_number=case_when(
    replied==1 ~ as.numeric(scheduled_message_number),
    TRUE ~ as.numeric(NA)
  )) %>%
  group_by(student_id, scheduled_message_count) %>%
  # By student, count up reply metrics
  summarize(scheduled_replies=sum(replied),
            scheduled_reply_proportion=sum(replied)/max(scheduled_message_count),
            first_scheduled_reply=min(reply_number, na.rm=TRUE),
            last_scheduled_reply=max(reply_number, na.rm=TRUE)) %>%
  ungroup() %>%
  # Clean up values for first and last scheduled reply
  mutate(first_scheduled_reply=case_when(
    first_scheduled_reply==Inf ~ as.numeric(NA),
    TRUE ~ first_scheduled_reply
  ),
  last_scheduled_reply=case_when(
    last_scheduled_reply==-Inf ~ as.numeric(NA),
    TRUE ~ last_scheduled_reply
  ),
  # Calculate engagement proportion variables
  prop_msgs_to_engagement=first_scheduled_reply/scheduled_message_count,
  prop_msgs_to_lastengagement=last_scheduled_reply/scheduled_message_count,
  engagement_duration_prop_msgs=(last_scheduled_reply-first_scheduled_reply+1)/scheduled_message_count)

# Calculate response times
scheduled_response_times_text <- texts %>%
  filter(student_reply==1 & !is.na(template_id2)) %>%
  group_by(student_id, template_id2) %>%
  arrange(time) %>%
  # Get only the student's first reply to the scheduled messages
  slice_head(n=1) %>%
  ungroup() %>%
  mutate(scheduled_response_time=as.numeric(dseconds(as.duration(time-scheduled_message_time)), "hours")) %>%
  # Don't count it as a response if it is more than 2 weeks later
  filter(scheduled_response_time<336) 

# Make a summary dataset by student for scheduled message response times
scheduled_response_times <- scheduled_response_times_text %>%
  group_by(student_id) %>%
  summarize(scheduled_response_time=mean(scheduled_response_time))

# Pare down this dataset to the scheduled message level
scheduled_response_times_text <- scheduled_response_times_text %>%
  select(student_id, institution, template_id2, scheduled_response_time) %>%
  distinct()

# Now calculate response times to any advisor messages
all_response_times_text <- texts %>%
  group_by(student_id, template_id2) %>%
  mutate(any_responses=mean(student_reply)) %>%
  filter(any_responses>0 & !is.na(template_id2)) %>%
  # Get the time of the previous message sent
  mutate(initial_message_time=lag(time, n=1)) %>%
  # Focus only on student messages
  filter(template_id=="" & message_type=="received") %>%
  mutate(all_response_time=as.numeric(dseconds(as.duration(time-initial_message_time)), "hours")) %>%
  # Again, don't count if the reply is more than two weeks later
  filter(all_response_time<336) 

# Get an aggregate student-level dataset for all response times
all_response_times <- all_response_times_text %>%
  group_by(student_id) %>%
  summarize(all_response_time=mean(all_response_time))

# Get the response times by scheduled messages
all_response_times_text <- all_response_times_text %>%
  group_by(student_id, template_id2, institution) %>%
  summarize(all_response_time=mean(all_response_time)) %>%
  select(student_id, institution, template_id2, all_response_time) %>%
  distinct()

# Get the timing of scheduled messages
scheduled_message_times <- template_order %>%
  group_by(student_id) %>%
  summarize(first_message_time = min(scheduled_message_time),
            last_message_time = max(scheduled_message_time),
            message_duration_hours= as.numeric(dseconds(as.duration(last_message_time-first_message_time+1)), "hours")) %>%
  select(student_id, first_message_time, message_duration_hours)

# Now calculate the times to engagement and times of engagement
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

# Make a quick dictionary of help-seeking language
help_phrases <- c("can you", "do you", "how do", "do i", "tell me", "where do", "can i",
                  "what do", "who do", "who is", "how to", "need help", "help me", "a question", "have questions",
                  "is there", "confused", "having trouble", "worried that", "not sure", "what are", "unable to",
                  "some help", "like to", "whether"
)

# Calculate whether a message has any help-asking language
number_of_help_asks_text <- texts %>%
  filter(message_type=="received") %>%
  mutate(questionmarks=as.numeric(str_detect(str_to_lower(text), "\\?")),
         help_phrase=as.numeric(str_detect(str_to_lower(text), paste(help_phrases, collapse="|")))) %>%
  rowwise() %>%
  mutate(any_help=max(questionmarks, help_phrase)) %>%
  ungroup() 

# Calculate the total number of help asks at the student-level
number_of_help_asks <- number_of_help_asks_text %>%
  group_by(student_id) %>%
  summarize(help_ask_messages=sum(any_help),
            help_messages_prop=mean(any_help))

# Calculate the number of help asks at the scheduled message level
number_of_help_asks_text <- number_of_help_asks_text %>%
  group_by(student_id, template_id2, institution) %>%
  summarize(help_ask_messages_count=sum(any_help),
            help_messages_prop=mean(any_help),
            any_help_asks=max(any_help)) %>%
  select(student_id, institution, template_id2, help_ask_messages_count, help_messages_prop, any_help_asks) %>%
  distinct()

# Get all the texts of a given conversation (scheduled message level) together
convosdata_text <- texts %>%
  filter(template_id=="" & !is.na(template_id2) & message_type=="received") %>% 
  group_by(student_id, template_id2) %>%
  mutate(all_text = paste(text, collapse=" ")) %>%
  add_tally() %>%
  slice(1) %>%
  ungroup() %>%
  select(student_id, template_id2, institution, n, all_text)

# Count up the word length
convosdata_text$convo_length_words <- sapply(gregexpr("[[:alpha:]]+", convosdata_text$all_text), function(x) sum(x > 0))

# Get a student-level dataset for the conversation lengths
convosdata <- convosdata_text %>%
  select(-all_text) %>%
  group_by(student_id) %>%
  summarize(avg_convo_length_student_words=mean(convo_length_words),
            avg_convo_length_student_msgs=mean(n))

# Get a scheduled-message-level dataset for the conversation lengths
convosdata_text <- convosdata_text %>%
  select(student_id, institution, template_id2, convo_length_words, n) %>%
  rename(convo_length_student_words=convo_length_words, convo_length_student_msgs = n) %>%
  distinct()

# Focus only on student replies
reply_lengths_text <- texts %>%
  filter(message_type=="received" & !is.na(template_id2))

# Count up the length of student replies in words
reply_lengths_text$reply_length_words <- sapply(gregexpr("[[:alpha:]]+", reply_lengths_text$text), function(x) sum(x > 0))

# Calculate at the student-level the length of their student replies
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

# Get a scheduled-message-level dataset for student reply lengths
reply_lengths_text <- reply_lengths_text %>%
  mutate(substantive_reply=case_when(
    reply_length_words>=5 ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(student_id, institution, template_id2) %>%
  summarize(substantive_reply_count=sum(substantive_reply),
            any_substantive_reply=max(substantive_reply))

# Put together a student-level output dataset for all engagement measures
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

# Put together a scheduled-message-level dataset for all engagement measures
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
