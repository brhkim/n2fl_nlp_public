################################################################################
#  
#   Name: 05a_topic_model_training.R
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

pacman::p_load(tidyverse, haven, lubridate, stm, tidytext, furrr, ggthemes, ggridges, quanteda, tm)

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
texts <- read_dta(file.path(project_data, '02_advisornames_cleaned.dta')) %>%
  select(student_id, template_id, text, time)

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

# texts_all <- texts %>%
#   group_by(student_id) %>%
#   mutate(all_text = paste(text, collapse=" ")) %>%
#   slice(1) %>%
#   ungroup()

texts <- texts %>%
  mutate(template_id2 = template_id)
texts$template_id2[texts$template_id2==""] <- NA

texts <- texts %>% 
  arrange(student_id, time) %>%
  group_by(student_id) %>%
  fill(template_id2) %>%
  ungroup()

cohort <- texts %>%
  filter(template_id!="") %>%
  select(student_id, time) %>%
  group_by(student_id) %>%
  mutate(earliest=min(time)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(cohort=case_when(earliest<as_date("2018-07-01") ~ "Spring 2018",
                         earliest>as_date("2018-07-01") & earliest<as_date("2019-01-01") ~ "Fall 2018",
                         earliest>as_date("2019-01-01") ~ "Spring 2019")) %>%
  select(student_id, cohort)

# Filter down to only custom texts
texts_custom <- texts %>%
  filter(template_id=="") %>% 
  group_by(student_id, template_id2) %>%
  mutate(all_text = paste(text, collapse=" ")) %>%
  add_tally() %>%
  slice(1) %>%
  ungroup() %>%
  mutate(student_id2 = paste0(student_id, "_", template_id2))

texts_custom <- texts_custom %>%
  # Replace email addresses
  mutate(all_text=str_replace_all(all_text, "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "tokenemailaddress")) %>%
  # Replace URLs
  mutate(all_text=str_replace_all(all_text, "(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)", "tokenurl")) %>%
  # Replace phone numbers
  mutate(all_text=str_replace_all(all_text, "([\\(]?[0-9]{3}[-\\s\\(\\)])?[0-9]{3}[-\\s\\(\\)][0-9]{4}", "tokenphonenumber"))

texts_custom_merge <- texts_custom %>%
  select(student_id2, all_text) %>%
  rename(docnames=student_id2)

# Tokenize and remove stop-words
tidy_texts_custom <- texts_custom %>%
  unnest_tokens(input=all_text, output=word, token="tweets", drop=TRUE) %>%
  anti_join(stop_words) 

# Remove numbers more generally
nums <- tidy_texts_custom %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
tidy_texts_custom <- tidy_texts_custom %>%
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

othernames <- c("laguardia", "ny", "john", "jay", "austin", "kingsborough", "permian", "basin", "arlington", "texas", "jj" ,"kcc", "utsa", "leh", "tamu", "ut", "virginia", "tyler", "nelson", "thomas", "mountain", "empire", "piedmont", "wytheville", "germanna")
othernames <- data.frame(word=othernames)

joinednames <- bind_rows(schoolnames, systemnames, othernames)

tidy_texts_custom <- tidy_texts_custom %>%
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
  tidy_texts_custom <- tidy_texts_custom %>%
    mutate(word=case_when(word==stringtext ~ replacementtext,
                          TRUE ~ word))
}

custom_dfm <- tidy_texts_custom %>%
  count(student_id2, word, sort = TRUE) %>%
  cast_dfm(document=student_id2, term=word, value=n) %>%
  dfm_trim(min_docfreq=10, docfreq_type="count") %>%
  dfm_subset(ntoken(.)>0)

retokenized <- tidy_texts_custom %>%
  select(student_id2, word) %>%
  group_by(student_id2) %>% 
  mutate(all_text=paste(word, collapse=" ")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-word)

tidy_texts_custom2 <- retokenized %>%
  unnest_tokens(input=all_text, output=word, token="ngrams", n=2, drop=TRUE)

custom_dfm2 <- tidy_texts_custom2 %>%
  count(student_id2, word, sort = TRUE) %>%
  filter(!is.na(word)) %>% # Remove this to reintroduce NAs
  cast_dfm(document=student_id2, term=word, value=n) %>%
  dfm_trim(min_docfreq=20, docfreq_type="count") %>%
  dfm_subset(ntoken(.)>0)

missingdata <- bind_rows(custom_dfm@docvars, custom_dfm2@docvars) %>%
  select(docname_) %>%
  mutate(student_id=str_split_fixed(docname_,'_\"', 2)[,1]) %>%
  left_join(metadata, by="student_id") %>%
  filter(is.na(treatment))

final_dfm <- rbind(custom_dfm, custom_dfm2) %>% 
  dfm_compress(margin="documents") %>%
  # Manually removing erroneous entry with no metadata
  dfm_subset(!str_split_fixed(docnames(.),'_\"', 2)[,1] %in% missingdata$student_id) %>%
  dfm_subset(str_split_fixed(docnames(.),'_\"', 2)[,1]!="314649")

metadata_prep <- data.frame(docnames=docnames(final_dfm),
                            student_id=str_split_fixed(docnames(final_dfm),'_\"', 2)[,1])

metadata_stm <- left_join(metadata_prep, metadata, by="student_id") %>%
  mutate(treatment=treatment-1,
         over_23=case_when(age_entry >= 24 ~ 1,
                           TRUE ~ 0)) 

text_times <- texts_custom %>%
  select(student_id2, time) %>%
  rename(docnames=student_id2) %>%
  mutate(time=as_datetime(time))

metadata_stm <- left_join(metadata_stm, text_times, by="docnames") %>%
  left_join(cohort, by="student_id")

final_stm <- asSTMCorpus(final_dfm, data=metadata_stm)

tokencount <- data.frame(docnames=names(ntoken(final_dfm)), tokens=ntoken(final_dfm))

# Save checkpoint
datafiles1 <- c("texts_custom_merge", "custom_dfm", "custom_dfm2", "final_stm", "final_dfm", "metadata_stm", "tokencount")
base::save(list=datafiles1, file=file.path(project_data, paste0('05a_topic_model_training_temp1', filesuffix, '.RData')))

################################################################################
#
#   #model - Start basic topic modeling and diagnostics
#
################################################################################

base::load(file=file.path(project_data, paste0('05a_topic_model_training_temp1', filesuffix, '.RData')))

set.seed(83191)
seed_root <- 83191

plan(multisession)

many_models <- data_frame(K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) %>%
  mutate(topic_model = future_map(K, ~stm(documents=final_stm$documents, vocab=final_stm$vocab, data=final_stm$data, 
                                          prevalence=~institution+system+treatment+male+afam+hisp+other+race_missing+over_23+degree_intent_BA_bline+transferee_ever_bline, K = .,
                                          verbose = FALSE), .options = furrr_options(seed = 83191)))

heldout <- make.heldout(documents=final_stm$documents, vocab=final_stm$vocab)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, final_dfm),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, final_dfm),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics")
ggsave(file.path(project_output, paste0('05a_topic_model_fit_metrics', filesuffix, '.png')), width=10, height=6)

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(20, 25, 30, 35, 40)) %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")
ggsave(file.path(project_output, paste0('05a_topic_model_exclusivity_coherence', filesuffix, '.png')), width=10, height=6)


################################################################################
#
#   #modelselection - Pick a model and prep the output for interpretation
#
################################################################################

topic_model <- k_result %>% 
  filter(K == 30) %>% 
  pull(topic_model) %>% 
  .[[1]]

colnames_reset <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "topic", "metric")
labeledtopics <- labelTopics(topic_model, n=10)
labeloutput_prob <- as.data.frame(labeledtopics[1]) %>%
  mutate(topic=rownames(.),
         metric="prob")
colnames(labeloutput_prob) <- colnames_reset

labeloutput_frex <- as.data.frame(labeledtopics[2]) %>%
  mutate(topic=rownames(.),
         metric="frex")
colnames(labeloutput_frex) <- colnames_reset

labeloutput <- bind_rows(labeloutput_prob, labeloutput_frex) %>%
  mutate(description="",
         topic=as.numeric(topic)) %>%
  arrange(topic) %>%
  relocate(topic, description, metric)

write_csv(labeloutput, file.path(project_data, paste0('05a_topic_codes_empty', filesuffix, '.csv')))

td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")
ggsave(file.path(project_output, paste0('05a_topic_model_words_by_topic', filesuffix, '.png')), width=10, height=6)

td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(final_dfm)) %>%
  left_join(tokencount, by=c("document"="docnames"))

# Set up output to show word frequency by topics
top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols=c(terms))

gamma_terms <- td_gamma %>%
  mutate(words=gamma*tokens) %>%
  group_by(topic) %>%
  summarise(words=sum(words)) %>%
  arrange(desc(words)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, words))

gamma_terms %>%
  ggplot(aes(topic, words, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 100, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 20000)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = "Number of Word Occurrences",
       title = "Topics by Frequency of Word Occurrences in the N2FL Data",
       subtitle = "Annotated with each topic's highest probability words")
ggsave(file.path(project_output, paste0('05a_topic_model_topic_prevalence', filesuffix, '.png')), width=10, height=6)

# Set up graphs to show word frequency by supertopics

gamma_terms2 <- td_gamma %>%
  mutate(supertopic=case_when(topic %in% c(9, 15, 18, 20, 21, 22, 25, 28, 30) ~ "Meeting Logistics",
                            topic %in% c(10, 6, 7, 8, 12) ~ "Course Planning",
                            topic %in% c(2, 23) ~ "Financial Aid",
                            topic %in% c(3, 4, 14, 17) ~ "Academic Resources",
                            topic %in% c(5, 13, 16, 19, 24, 26) ~ "Academic Planning",
                            topic %in% c(1, 11, 27, 29) ~ "Other",
                            TRUE ~ "Other")) %>%
  mutate(words=gamma*tokens) %>%
  group_by(topic) %>%
  summarise(words=sum(words),
            supertopic=first(supertopic)) %>%
  arrange(desc(words)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, words))

gamma_terms2 %>%
  ggplot(aes(topic, words, label = terms, fill = supertopic)) +
  geom_col() +
  geom_text(hjust = 0, nudge_y = 100, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 23000)) +
  scale_fill_colorblind() +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = "Number of Word Occurrences", fill="Supertopic",
       title = "Topics and Supertopics by Frequency of Word Occurrences in the N2FL Data",
       subtitle = "Annotated with each topic's highest probability words")
ggsave(file.path(project_output, paste0('05a_topic_model_supertopics_prevalence', filesuffix, '.png')), width=10, height=6)

corroutput <- topicCorr(topic_model)

datafiles2 <- c("many_models", "heldout", "k_result", "topic_model", "labeloutput", "td_beta", "td_gamma", "top_terms", "gamma_terms", "gamma_terms2", "corroutput")
base::save(list=datafiles2, file=file.path(project_data, paste0('05a_topic_model_training_temp2', filesuffix, '.RData')))

base::load(file=file.path(project_data, paste0('05a_topic_model_training_temp2', filesuffix, '.RData')))


# Effect estimation via the stm package; deprecated for now BK 21-02-02
# prep <- estimateEffect(1:25 ~ afam, topic_model, 
#                        meta = final_stm$data, uncertainty = "Global")
# 
# plot(prep, covariate = "afam", topics = c(1:25),
#      model = topic_model, method = "difference",
#      cov.value1 = "0", cov.value2 = "1",
#      xlab = "More Conservative ... More Liberal",
#      xlim = c(-.1, .1))


# ~institution+system+treatment+male+afam+hisp+other+race_missing+over_23_degree_intent_BA_bline+transferee_ever_bline


################################################################################
#
#   #output - Get the output
#
################################################################################

# BK: Deprecated! Use 05b_topic_modeling.R instead to get accurate readings
# base::load(file=file.path(project_data, paste0('05a_topic_model_training_temp1', filesuffix, '.RData')))
# base::load(file=file.path(project_data, paste0('05a_topic_model_training_temp2', filesuffix, '.RData')))
# 
# output <- td_gamma %>%
#   pivot_wider(id_cols=c(document, tokens), names_from=topic, names_prefix="t", values_from=gamma) %>%
#   rename(docnames=document) %>%
#   mutate(meet_prop=t9+t15+t18+t20+t21+t22+t25+t28+t30,
#          crse_prop=t10+t6+t7+t8+t12,
#          fina_prop=t2+t23,
#          plan_prop=t5+t13+t16+t19+t24+t26,
#          reso_prop=t3+t4+t14+t17,
#          gene_prop=t1+t11+t27+t29) %>%
#   mutate(meet_count=meet_prop*tokens,
#          crse_count=crse_prop*tokens,
#          fina_count=fina_prop*tokens,
#          plan_count=plan_prop*tokens,
#          reso_count=reso_prop*tokens,
#          gene_count=gene_prop*tokens) %>%
#   left_join(metadata_stm, by="docnames") %>%
#   mutate(template_id=str_split_fixed(docnames,'_\"', 2)[,2]) %>%
#   left_join(texts_custom_merge, by="docnames") %>%
#   mutate(all_text=substr(all_text, 1, 1024))
# 
# validate <- output %>%
#   mutate(token_total=meet_count+crse_count+fina_count+plan_count+reso_count+gene_count,
#          diff=tokens-token_total,
#          prop_total=meet_prop+crse_prop+fina_prop+plan_prop+reso_prop+gene_prop)
# 
# summary(validate$prop_total)
# summary(validate$diff)
# 
# write_dta(output, file.path(project_data, paste0('05a_topics_cleaned', filesuffix, '.dta')))


################################################################################
#
#   #basicviz - Conduct some basic visualization of the output
#
################################################################################

# graphdata <- output %>%
#   filter(system=="CUNY" & cohort=="Fall 2018") %>% 
#   mutate(year=year(time),
#          week=week(time)) %>%
#   mutate(date2=ymd("2014-01-01") + weeks(week - 1) + years(year - 2014)) %>%
#   group_by(date2) %>%
#   summarize(meet_count=sum(meet_count),
#             crse_count=sum(crse_count),
#             fina_count=sum(fina_count),
#             plan_count=sum(plan_count),
#             reso_count=sum(reso_count),
#             gene_count=sum(gene_count),
#             tokens=sum(tokens)) %>%
#   ungroup() %>%
#   mutate(p_meet=meet_count/tokens,
#          p_crse=crse_count/tokens,
#          p_fina=fina_count/tokens,
#          p_plan=plan_count/tokens,
#          p_reso=reso_count/tokens,
#          p_gene=gene_count/tokens,) %>%
#   pivot_longer(cols=starts_with("p_"), names_to="supertopic", values_to="prop")
# 
# graphdata %>%
#   filter(supertopic=="p_meet") %>%
#   ggplot(aes(x=date2, y=prop)) +
#     geom_histogram(stat="identity") + 
#     theme_minimal() + 
#     scale_x_date(date_breaks="1 month", date_labels="%B %Y") +
#     theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0)) + 
#     labs(x="\nDate", y="Proportion of Words\n", title="Supertopic Prevalence Over Time: Meeting Scheduling Logistics", subtitle="For the Fall 2018 Cohort at CUNY Institutions")
# ggsave(file.path(project_output, paste0('05a_topic_model_meet_over_time', filesuffix, '.png')), width=10, height=6)
# 
# graphdata %>%
#   filter(supertopic=="p_crse") %>%
#   ggplot(aes(x=date2, y=prop)) +
#   geom_histogram(stat="identity") + 
#   theme_minimal() + 
#   scale_x_date(date_breaks="1 month", date_labels="%B %Y") +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0)) + 
#   labs(x="\nDate", y="Proportion of Words\n", title="Supertopic Prevalence Over Time: Course Registration", subtitle="For the Fall 2018 Cohort at CUNY Institutions")
# ggsave(file.path(project_output, paste0('05a_topic_model_crse_over_time', filesuffix, '.png')), width=10, height=6)
# 
# graphdata %>%
#   filter(supertopic=="p_fina") %>%
#   ggplot(aes(x=date2, y=prop)) +
#   geom_histogram(stat="identity") + 
#   theme_minimal() + 
#   scale_x_date(date_breaks="1 month", date_labels="%B %Y") +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0)) +  
#   labs(x="\nDate", y="Proportion of Words\n", title="Supertopic Prevalence Over Time: Financial Aid", subtitle="For the Fall 2018 Cohort at CUNY Institutions")
# ggsave(file.path(project_output, paste0('05a_topic_model_fina_over_time', filesuffix, '.png')), width=10, height=6)
# 
# graphdata %>%
#   filter(supertopic=="p_acad") %>%
#   ggplot(aes(x=date2, y=prop)) +
#   geom_histogram(stat="identity") + 
#   theme_minimal() + 
#   scale_x_date(date_breaks="1 month", date_labels="%B %Y") +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0)) + 
#   labs(x="\nDate", y="Proportion of Words\n", title="Supertopic Prevalence Over Time: General Academic Support and Planning", subtitle="For the Fall 2018 Cohort at CUNY Institutions")
# ggsave(file.path(project_output, paste0('05a_topic_model_acad_over_time', filesuffix, '.png')), width=10, height=6)

