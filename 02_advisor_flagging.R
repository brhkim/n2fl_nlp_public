################################################################################
#  
#   Name: 02_advisor_flagging_a.R
#   
#   Purpose: Get advisor names out of text data from N2FL texting intervention.
#
#
################################################################################


################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

libs <- c('tidyverse','ggplot2','haven','readxl', 'stringr')
sapply(libs, require, character.only = TRUE)

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
texts <- read_dta(file.path(project_data, '01_combined.dta'))


################################################################################
#
#   #split - Split out advisor names
#
################################################################################

# Check number of duplicates for each text
texts_unique <- texts %>% 
  filter(template_id!="") %>% 
  group_by(institution, text) %>%
  mutate(dupes = n()) %>% # Count how many copies of that text there are
  ungroup() %>%
  group_by(institution, template_id) %>% 
  mutate(template_dupes = n()) %>% # Count how many copies of that template_id there are
  ungroup() %>%
  mutate(proportions = dupes/template_dupes) %>% # Generate a proportion of the above two values
  select(institution, template_id, text, dupes, template_dupes, proportions) %>%
  distinct()

# Split texts and take only the first 60 characters
texts_unique <- texts_unique %>%
  mutate(starttext = str_sub(text, 0, 60))

# Get rid of titles
texts_unique$starttext <- str_replace(texts_unique$starttext, "Prof. ", "")
texts_unique$starttext <- str_replace(texts_unique$starttext, "Dr. ", "")
texts_unique$starttext <- str_replace(texts_unique$starttext, "Mr. ", "")
texts_unique$starttext <- str_replace(texts_unique$starttext, "Ms. ", "")
texts_unique$starttext <- str_replace(texts_unique$starttext, "Mrs. ", "")

# Make a dataset split on greetings typically preceding the advisor's name
texts_split <- str_split_fixed(texts_unique$starttext, "it's|I'm|this is", 3) %>%
  as.data.frame()

texts_split_name <- str_split_fixed(texts_split$V2, "\\,|\\!|\\.|[:space:][:lower:]", 5) %>%
  as.data.frame() %>%
  select(V1) %>%
  rename(advisorname = V1) %>%
  mutate(advisorname = str_trim(advisorname))

observe <- texts_split_name %>%
  select(advisorname) %>%
  distinct() %>%
  arrange(advisorname)

# Join it back in
names <- bind_cols(texts_unique, texts_split_name) %>%
  select(institution, text, advisorname)

output <- left_join(texts, names, by=c("institution", "text"))

# Replace any NA's
output$advisorname[is.na(output$advisorname)] <- ""

# Clean up the text field for names
cleaner_prep <- output %>% 
  filter(advisorname!="") %>% 
  select(advisorname) %>%
  distinct()

# Make a list of name-related words that *are not* already in the advisornames list that
# should still be removed from the texts
cleaner_list <- c("Prof. ",
             "Mr. ",
             "Mrs. ",
             "Ms. ",
             "Dr. ",
             "NAMEer",
             "NAMEa",
             "NAMEh",
             "NAMEe",
             "NAME") %>%
  as.data.frame()
colnames(cleaner_list) <- "advisorname"

cleaner <- bind_rows(cleaner_prep, cleaner_list)

cleaner <- cleaner$advisorname %>%
  str_c(collapse="|")

# Remove advisornames and NAME tokens from texts
output$cleantext <- output$text %>% 
  str_replace_all(cleaner, "") %>%
  str_trim(side="both") %>%
  str_squish()

# Remove now-empty salutations and erroneous punctuation
output$cleantext <- str_replace(output$cleantext, ", it's \\.", ".")
output$cleantext <- str_replace(output$cleantext, ", it's \\!", "!")
output$cleantext <- str_replace(output$cleantext, ", I'm \\.", ".")

output$cleantext <- str_replace(output$cleantext, ", this is \\.", ".")
output$cleantext <- str_replace(output$cleantext, ", this is \\!", "!")

output$cleantext <- str_replace(output$cleantext, " it's \\!", "!")
output$cleantext <- str_replace(output$cleantext, " it's \\.", ".")
output$cleantext <- str_replace(output$cleantext, " I'm \\.", ".")
output$cleantext <- str_replace(output$cleantext, " I'm \\!", "!")

output$cleantext <- str_replace(output$cleantext, ", I'm \\,", ", I'm ")
output$cleantext <- str_replace(output$cleantext, " I'm \\,", " I'm ")

output$cleantext <- str_replace(output$cleantext, " \\.", ".")
output$cleantext <- str_replace(output$cleantext, " \\!", "!")
output$cleantext <- str_replace(output$cleantext, " \\,", ",")
output$cleantext <- str_replace(output$cleantext, " \\?", "?")

output$cleantext <- output$cleantext %>% 
  str_trim(side="both") %>%
  str_squish()

output$advisorname[output$advisorname==""] <- NA

output <- output %>%
  arrange(student_id, time) %>%
  group_by(student_id) %>%
  fill(advisorname, .direction="downup") %>%
  ungroup()

output_a <- output %>%
  rename(oldtext = text,
         text = cleantext)

# Save full dataset
write_dta(output_a, file.path(project_data, '02_advisornames_cleaned.dta'))

# Save concise dataset for masked sentiment analysis
output_b <- output_a %>%
  select(text) %>%
  distinct()
write_dta(output_b, file.path(project_data, '02_advisornames_cleaned_unique.dta'))

# While we're here: Quick check for number of gendered pronouns
pronounscheck <- output_b %>%
  mutate(he_count = str_count(text, " he "),
         she_count = str_count(text, " she "),
         her_count = str_count(text, " her "),
         his_count = str_count(text, " his "),
         him_count = str_count(text, " him "),
         hers_count = str_count(text, " hers "))

pronounscheck <- pronounscheck %>%
  mutate(total_count = he_count + she_count + her_count + his_count + him_count + hers_count,
         any_count = (total_count>0))

pronounscheck$total_count %>% sum(na.rm=TRUE)
pronounscheck$any_count %>% sum(na.rm=TRUE)


# Save concise dataset for unmasked sentiment analysis
output_c <- output_a %>%
  select(oldtext) %>%
  distinct() %>%
  rename(text = oldtext)
write_dta(output_c, file.path(project_data, '02_advisornames_uncleaned_unique.dta'))
