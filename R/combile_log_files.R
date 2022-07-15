# load packages
library(tidyverse)
library(openxlsx)
library(lubridate)

# todays date
today_dat <- as_date(today())

## standard columns for log
# uuid, type, names, value, index, relevant, checked_by, checked_date, comment


# load datasets to combine
# getwd()
data_files <- list.files(path = paste0(getwd(),"/inputs/"), pattern ="*._log_.*\\.csv$", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>%
  rename_all(recode, "Checked by"="Checked_by") %>% 
  mutate(
    start_date = Date,
    enumerator = Enumerator_name,
    name = Question_name,
    current_value = Current_value,
    value = Suggested_change,
    issue = Issue,
    checked_by = Checked_by,
    checked_date = today_dat,
    comment = Explanation
  ) %>% 
  select(
    uuid, start_date, enumerator, district_name, refugee_settle_name, name, 
    current_value, value, issue, checked_by, checked_date, comment
  )

data_files

# survey questions
survey_questions <- read_csv(file = "inputs/survey.csv") %>% 
  select( type,  name
  )
survey_questions


# join the log with the survey
data_log_survey <- left_join(data_files, survey_questions, 
                             by = "name", copy = FALSE, 
                             na_matches = c("na", "never")) %>% 
  mutate(
    question_type = type
  )%>% 
  select(uuid,start_date,enumerator,district_name,refugee_settle_name,
    question_type, name, current_value, value, issue, checked_by, checked_date,
    comment
  )
  
data_log_survey


# check provided choices
choices_data <- read_csv(file = "inputs/choices.csv") %>% 
  select(
    list_name,
    name
  )

choices_data
choices_list <- choices_data$name
choices_list


# function for getting choices for a particular question
choiceListForQuestion <- function(cl_list, cl_df_with_type, cl_question){
  # use question to filter type
  list_type <- cl_df_with_type %>% 
    filter( name == cl_question ) %>% 
    select( question_type )%>% 
    head(n=1) %>% 
    str_split(" ")
  
  data_list_filter <- as.character(list_type[[1]])

  # then use type to filter choices
  choices_data <- cl_list %>% 
    select(list_name, name) %>% 
    filter(list_name == data_list_filter[2]) %>%  
    select(name)

  return(
    choices_data$name
  )
}


# vectorized function
choiceListForQuestion_V <- Vectorize(choiceListForQuestion)

choices_test_data <- read_csv(file = "inputs/choices.csv")
ttd <- choiceListForQuestion(choices_test_data, data_log_survey, cl_question="main_livelihood" )
ttd
# test other options given compared to available choices
# "change_response"
# "add_option" & "remove_options"
# str_detect
# |text
filtered_test_data <- data_log_survey %>%
  mutate(
    type = ifelse(grepl("_other$", name) & grepl("select_one|text", question_type) & current_value %in% choices_list, "change_response",
                  ifelse(grepl("_other", name) & grepl("select_mult",question_type) & current_value %in% choices_list, "remove_option", NA ))
  )
  

filtered_test_data



# filter is good. But the data has issues especially the current_value
filtered_test <- data_log_survey %>%
  filter(
    # str_detect(name, "_other$") &
    # current_value %in% c(choices_list) & str_detect(name,"text")
    # current_value %in% c(choices_list) & (str_detect(question_type,"_other") | str_detect(question_type,"text"))
    current_value %in% choices_list
  ) %>% 
filter(
 str_detect(question_type,"_other") | str_detect(question_type,"text")
)

filtered_test





# output the file
write_csv(filtered_test_data, file = paste0(getwd(),"/outputs/combined_logs_survey_test_",today_dat,".csv"), na="")






# checking function implimentation
data_log_survey$name
filtered_data <- data_log_survey %% 
  mutate(
    type = ifelse(grepl("_other$", name) & grepl("select_one|text", question_type) & value %in% lapply( choiceListForQuestion(choices_test_data, data_log_survey, name )), "change_response",
                  ifelse(grepl("_other", name) & grepl("select_mult",question_type) & value %in% lapply(choiceListForQuestion(choices_test_data, data_log_survey, name )), "remove_option", NA ))
  )

filtered_data