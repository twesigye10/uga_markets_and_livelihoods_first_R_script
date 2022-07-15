## load packages
library(tidyverse)
library(openxlsx)
library(lubridate)


## todays date
today_dat <- as_date(today())
today_hour <- hour(now())

## standard columns for log
# uuid, type, names, value, index, relevant, checked_by, checked_date, comment


## load datasets to combine
data_files <- list.files(path = paste0(getwd(),"/cleaning/"), pattern ="*._log_.*\\.csv$", full.names = TRUE) %>% 
  lapply(read_csv, col_types = cols(Date="c")) %>% 
  bind_rows() %>%
  rename_all(recode, "Checked by"="Checked_by") %>% 
  mutate(
    checked_date = Date,
    enumerator = Enumerator_name,
    name = Question_name,
    current_value = Current_value,
    suggested_change = Suggested_change,
    issue = Issue,
    checked_by = Checked_by,
    comment = Explanation
  ) %>% 
  select(
    uuid, checked_date, enumerator, district_name, refugee_settle_name, name, 
    current_value, suggested_change, issue, checked_by, comment
  )

# data_files

## survey questions
survey_questions <- read_csv(file = "inputs/survey.csv") %>% 
  select(type,name
  )
# survey_questions

## join the log with the survey
data_log_survey <- left_join(data_files, survey_questions, 
                             by = "name", copy = FALSE, 
                             na_matches = c("na", "never")) %>% 
  mutate(
    question_type = type
  )%>% 
  select(
    uuid,
    enumerator,
    district_name,
    refugee_settle_name,
    question_type,
    name,
    current_value,
    suggested_change,
    issue,
    checked_by,
    checked_date,
    comment
  )

# data_log_survey


####     options for type_of_change  end   ####

# remove_survey
# "change_response"
# "add_option" & "remove_option"

####     options for type_of_change  end   ####


####     present values  start   ####     
# bad_weather
# remove response
# few_animals
# no_support_needed
# land_insufficient
# both_food_y

# no change(ignore)
# Confirmed(ignore)
# none(ignore)

# small_bus_sales
# gfa(cash)
# many_job high_wag no_large_invest
# many_job
# it_is_safe
# yes
# no_challenge
# provide_food_y
# no_through_other_sources
# 0
# no
# remittance_gifts
# change to the option

####     present values  end   ####  


data_add_columns <- data_log_survey %>%
  filter(
    !(grepl("no change|no_change|Confirmed|rectified", suggested_change, ignore.case=TRUE) | grepl("rectified", comment, ignore.case=TRUE) | (grepl("same", issue, ignore.case=TRUE) & grepl("select_multi", question_type)) )
  ) %>% 
  mutate(
    type = ifelse(grepl("bad weather|bad_weather", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)) , "change_response",
                  ifelse(grepl("remove respo|remove_respo", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "remove_option", 
                  ifelse(grepl("few_animals|few animals", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("no_support_needed|no support needed", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("land_insufficient|land insufficient", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("both_food_y|both food y", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("none", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("small_bus_sales|small bus sales", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("gfa(cash)", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("many_job|many job", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("it_is_safe|it is safe", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("yes", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("no_challenge|no challenge", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("provide_food_y|provide food y", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("no_through_other_sources|no through other sources", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("no", suggested_change) & (grepl("select_one", question_type, ignore.case=TRUE)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("remittance_gifts|remittance gifts", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("change to the option|change_to_the_option", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("int", question_type) & grepl("[0-9]", suggested_change), "change_response",  
                  ifelse(grepl("delete respo|delete_respo|delete surv|delete_surv", suggested_change, ignore.case=TRUE), "remove_survey",
                  ifelse(grepl("few_members_work|few members work", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("small_bus_service|small bus service", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("transport|transport", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("low_px_gds_sold|low px gds sold", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("Change response|Change response", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("bricklaying|no brick laying", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("larger_bus_sales|larger bus sales", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("farming_own_land|farming own land", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("Bee_keeping|Bee keeping", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                  ifelse(grepl("other", suggested_change, ignore.case=TRUE) & (grepl("select_one", question_type)|grepl("text", question_type)), "change_response", 
                         NA 
                         ))))))))))))))))))))))))))))))
  ) %>% 
  mutate(
    value = ifelse(grepl("change_response|remove_option|add_option", type, ignore.case=TRUE), str_replace(suggested_change, " ", "_"), NA
    )
  ) %>%
  
  ## uuid, type, names, value, index, relevant, checked_by, checked_date, comment
  select(
    enumerator,
    district_name,
    refugee_settle_name,
    question_type,
    current_value,
    suggested_change,
    issue,
    uuid,
    type,
    name,
    value,
    checked_by,
    checked_date,
    comment
  )
  
data_add_columns



## first flag the removal of the option for select_multiple
data_multi_select_remove_option <- data_add_columns %>% 
  mutate(
    type = ifelse(grepl("select_multi", question_type) & grepl("option exists|option_exists", issue, ignore.case=TRUE) , "remove_option", type),
    value = str_replace(suggested_change, " ", "_")
  ) %>% 
  mutate(
    type = ifelse(grepl("select_multi", question_type) & grepl("none|remove response|remove_response", suggested_change, ignore.case=TRUE) , "remove_option", type),
    value = str_replace(current_value, " ", "_")
  ) %>% 
  mutate(
    type = ifelse(grepl("delete respo|delete_respo|delete surv|delete_surv", suggested_change, ignore.case=TRUE) , "remove_survey", type),
  ) 
  
data_multi_select_remove_option

## first flag the addition of the option for select_multiple
data_multi_select_add_option <- data_multi_select_remove_option %>% 
  filter(
    (grepl("select_multi", question_type) & grepl("option exists|option_exists", issue, ignore.case=TRUE) ) 
  ) %>% 
  mutate(
    type = "add_option",
    value = str_replace(suggested_change, " ", "_")
  ) 

## merge the datasets
data_cleaned_multi_select <-  rbind(data_multi_select_remove_option, data_multi_select_add_option) %>% 
  mutate(
    value = ifelse(grepl("delete respo|delete_respo|delete surv|delete_surv", suggested_change, ignore.case=TRUE), 
                   ifelse(grepl("delete respo|delete_respo|delete surv|delete_surv", suggested_change, ignore.case=TRUE)&(!grepl("delete respo|delete_respo|delete surv|delete_surv", current_value, ignore.case=TRUE)), NA, current_value)
                   , suggested_change)
  ) %>%
  mutate(
    value = ifelse(grepl("remove option|remove_option", type, ignore.case=TRUE) & grepl("remove respo|remove_respo", suggested_change, ignore.case=TRUE),  current_value, value)
  ) %>%
  arrange_at(
    vars(checked_date,uuid)
  )
data_cleaned_multi_select

# output the file same
write_csv(data_cleaned_multi_select, file = paste0(getwd(),"/outputs/combined_logs_with_template_",today_dat,"_",today_hour,"_",".csv"), na="")





# 
# # # testing filter
# filtered_data <- data_log_survey %>%
#   filter(
#     # !(grepl("no change|Confirmed|rectified", suggested_change, ignore.case=TRUE) | grepl("rectified", comment, ignore.case=TRUE) | (grepl("same", issue, ignore.case=TRUE) & grepl("select_multi", question_type)) )
#     grepl("int", question_type) & grepl("^[0-9]", suggested_change)
#   )
# filtered_data
