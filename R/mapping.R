
## load packages
library(tidyverse)
library(leaflet)
library(htmltools)
library(DT)
library(crosstalk)


# import data
df_raw <- read_csv(file = "outputs/checked_data_issues_2021-03-03_9.csv") %>%
  select(
    uuid,
    district_name,
    sub_county_name,
    status,
    refugee_settle_name,
    survey_time_interval,
    time_between_survey,
    found_issue,
    today,
    point_number,
    enumerator,
    geopoint
  ) %>% 
  separate(geopoint, c("latitude", "longitude"), " ", remove= FALSE, extra = "drop")
  
# df_raw

# 
# pal <- colorFactor(c("red", "orange", "yellow" ), domain = c("duplicate_pt_no", "less_survey_time", "less_time_btn_surveys"))
# 
# challenges_map <- df_raw %>%
#   leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(~as.numeric(longitude),
#              ~as.numeric(latitude),
#              popup=~found_issue,
#              radius = 10,
#              color = ~pal(found_issue),
#              stroke = FALSE, fillOpacity = 0.9,
#              label = ~found_issue,
#              clusterOptions = markerClusterOptions()
#   ) %>% 
#   addLegend("bottomright", colors=c("red", "orange", "yellow" ), labels=c("duplicate_pt_no", "less_survey_time", "less_time_btn_surveys"), title="Errors from data")
# 
# 
# challenges_map



pal <- colorFactor(c("red", "orange", "yellow" ), domain = c("duplicate_pt_no", "less_survey_time", "less_time_btn_surveys"))

sdf <- SharedData$new(df_raw, df_raw$found_issue)

bscols(leaflet(sdf) %>% addTiles() %>%
         # addMarkers(~ as.numeric(longitude), ~ as.numeric(latitude)),
       addCircleMarkers(~as.numeric(longitude),
                        ~as.numeric(latitude),
                        popup=~found_issue,
                        radius = 10,
                        color = ~pal(found_issue),
                        stroke = FALSE, fillOpacity = 0.9,
                        label = ~found_issue,
                        clusterOptions = markerClusterOptions()) %>% 
         addLegend("bottomright", colors=c("red", "orange", "yellow" ), labels=c("Duplicate Point No", "Less Survey Time", "Less Time btn Surveys"), title="Errors from data")
       ,
       datatable(
         sdf,
         rownames = FALSE,
         colnames = c('UUID'='uuid', 'District'='district_name', 'Sub County'='sub_county_name', 'Status'='status', 'Settle Name'='refugee_settle_name', 'Survey Time'='survey_time_interval', 'Time BTN Survey'='time_between_survey', 
                      'Issue'='found_issue', 'Date'='today', 'Point Number'='point_number', 'Enumerator'='enumerator' ),
         width = "100%", 
         class = 'cell-border stripe'
         )
       )
  

# start,
# end,



# enum_id_refugee,
# enum_id_host,


# uuid,
# district_name,
# sub_county_name,
# status,
# refugee_settle_name,
# survey_time_interval,
# time_between_survey,
# found_issue,
# today,
# point_number,
# enumerator,
# geopoint,
