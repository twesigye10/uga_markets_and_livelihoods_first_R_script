## load packages
library(tidyverse)
library(lubridate)
library(openxlsx)
library(koboAPI)

source("support_files/credentials.R")

# ## todays date
# today_dat <- as_date(today())
# today_dat
# today_hour <- hour(now())
# data_collection_start <- as_date("2021-02-14")
# 
# df_raw <- read_csv(file = "inputs/UGA2101a_Rapid_Assessment_on_Livelihood_and_Market_Strengthening_Opportunities_Feb2021.csv") %>%
#   rename(
#     uuid = "_uuid",
#     index = "_index"
#   )
# 
# df_raw


## todays date
u <- user_name
pw <- password
form_id <- used_form_id
data <- download_data(form_id, user=u, pwd = pw)

today_dat <- as_date(today())
today_dat
today_hour <- hour(now())
data_collection_start <- as_date("2021-02-14")

colnames(data)

df_raw <- data %>%
  rename(
    uuid = "_uuid"
    # ,
    # index = "_index"
  )

df_raw


# settlements with more than 1 point based on point_number
df_check_duplicate_refugee_settlement_pts <-  df_raw %>% 
  filter(
    today > data_collection_start & status=="refugee_settlement"
  ) %>% 
  group_by(
    district_name, sub_county_name, point_number
  ) %>% 
  add_count(point_number) %>% 
  filter(
    n()>1 
  ) %>% 
  mutate(
    found_issue = "duplicate_pt_no"
  )

df_check_duplicate_refugee_settlement_pts

# host community with more than 6 points based on point_number
df_check_duplicate_host_community_pts <-  df_raw %>% 
  filter(
    today > data_collection_start & status=="host_community"
  ) %>% 
  group_by(
    district_name,sub_county_name, point_number
  ) %>% 
  add_count(point_number) %>% 
  filter(
    n()>6 
  ) %>% 
  mutate(
    found_issue = "duplicate_pt_no"
  )

df_check_duplicate_host_community_pts


# points collected over less time interval
df_check_less_survey_time <-  df_raw %>% 
  filter(
    today > data_collection_start
  ) %>% 
  mutate(
    survey_time_interval = difftime(end,start, units = "mins"),
    survey_time_interval = round(survey_time_interval,2)
  )%>% 
  filter(
    survey_time_interval < 20
  ) %>% 
  mutate(
    found_issue = "less_survey_time"
  )

df_check_less_survey_time


# points collected with less time between between surveys

df_check_less_time_between_survey_support <-  df_raw %>% 
  filter(
    today > data_collection_start
  ) %>%
  mutate(
    enum_id = ifelse(!is.na(enum_id_refugee) , enum_id_refugee, enum_id_host )
  ) %>%
  group_by(
    today,enum_id
  ) %>%
  arrange(start, .by_group = TRUE) %>% 
  mutate(
    t_between_survey = (start - lag(end, default=first(start))),
    # time_between_survey = t_between_survey/60
    time_between_survey = make_difftime(t_between_survey, units = "mins"),
    time_between_survey = round(time_between_survey,2)
  ) %>% 
  filter(
    time_between_survey !=0 & time_between_survey < 5
  ) %>%
  mutate(
    found_issue = "less_time_btn_surveys"
  )
  # %>%
  # select(
  #   start,
  #   end,
  #   time_between_survey,
  #   today,
  #   enumerator,
  #   enum_id_refugee,
  #   enum_id,
  #   enum_id_host,
  #   district_name,
  #   status,
  #   sub_county_name,
  #   point_number,
  #   uuid,
  #   index
  # ) %>% 
  # openxlsx::write.xlsx(file =paste0(getwd(),"/outputs/checked_data_less_time_btn_surveys_",today_dat,"_",hour(now()),".xlsx"),sheetName="time_btn_surveys", row.names = FALSE)

  
df_check_less_time_between_survey_support

# columns to return
# uuid,district_name,sub_county_name,refugee_settle_name,geopoint,enumerator_name
df_combined_issues <- rbind(df_check_duplicate_refugee_settlement_pts, df_check_duplicate_host_community_pts, df_check_less_survey_time, df_check_less_time_between_survey_support) %>% 
  select(
    start,
    end,
    survey_time_interval,
    time_between_survey,
    found_issue,
    today,
    enum_id_refugee,
    enum_id_host,
    uuid,
    district_name,
    sub_county_name,
    status,
    refugee_settle_name,
    point_number,
    geopoint,
    enumerator
  )


df_combined_issues

openxlsx::write.xlsx(df_combined_issues, file =paste0(getwd(),"/outputs/checked_data_issues_",today_dat,"_",hour(now()),".xlsx"),sheetName="surveys_with_issues", row.names = FALSE)
  

df_combined_issues %>%
  group_by(
    district_name, status,today
  ) %>%
  tally() %>%
  mutate(
    number_of_surveys = n
  ) %>%
  select(
    district_name, status, today, number_of_surveys
  ) %>%
  openxlsx::write.xlsx(file =paste0(getwd(),"/outputs/summary_data_issues_",today_dat,"_",hour(now()),".xlsx"),sheetName="surveys_with_issues", row.names = FALSE)


