library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "verbal_complexity"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

#code for dealing with atypical participant id storage
participant_ids <- exp_data %>%
  select(random_id,response) %>%
  filter(str_detect(response,"participant_id")) %>%
  #extract response to participant_id
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(cols = c(json)) %>%
  #clean up participant ids
  mutate(
    participant_id = case_when(
      participant_id == "9252" ~ "parrot",
      participant_id == "A18534325" ~ "moose",
      TRUE ~ trimws(tolower(participant_id))
    )
  ) %>%
  select(random_id,participant_id)

#join in to exp_data
exp_data <- exp_data %>%
  left_join(participant_ids,by="random_id")

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#extract responses
processed_data <- exp_data %>% 
  #filter data
  filter(trial_index %in% seq(4,25)) %>%
  select(-participant_gender) %>%
  rename(stimulus_list = stimulus) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  #remove unneeded columns
  select(-c(participant:failed_video,response_index,question_order)) %>%
  select(-c(success,plugin_version,response_time_seconds))

#grab participant_gender
participant_gender <- processed_data %>%
  filter(!is.na(participant_gender)) %>%
  distinct(random_id,participant_id,participant_gender)

participant_major <- processed_data %>%
  filter(!is.na(participant_major)) %>%
  distinct(random_id,participant_id,participant_major)

#join back in
processed_data <- processed_data %>%
  select(-c(participant_gender,participant_major)) %>%
  left_join(participant_gender) %>%
  left_join(participant_major) %>%
  #filter out demographics rows
  filter(!is.na(figure))

#filter participant ids
filter_ids <- c()

#identify participants from the experiment group
group_members <- c("hamster","goldfish","wombat","wolf","meerkat")

processed_data <- processed_data %>%
  filter(!(participant_id %in% filter_ids)) %>%
  #flag for group participants
  mutate(participant_is_group_member = case_when(
    participant_id %in% group_members ~ TRUE,
    TRUE ~ FALSE
  
  )) %>%
  #add trial_number
  group_by(participant_id) %>%
  mutate(trial_number = row_number()) %>%
  relocate(trial_number,.after=trial_index)

#extract the actual complexity value
processed_data <- processed_data %>%
  mutate(
    #remove stimuli/ and .jpg from stimuli/[figure_name].jpg
    stimulus_complexity = str_remove(figure,"stimuli/") %>% str_remove(".jpg") %>% as.numeric()
  )
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
