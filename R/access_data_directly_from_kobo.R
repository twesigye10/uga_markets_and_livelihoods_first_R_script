## Read data directly from submissions on Kobo

## load packages
library(tidyverse)
library(lubridate)
# Package ‘httr’ has very useful tools for working with HTTP requests such as GET, POST, PATCH etc. 
# In addition, we can pass parameters such as authentication information etc. 
# The ‘jsonlite’ package offers flexible, robust, high performance tools for working with JSON data
library(httr)
library(jsonlite)
#for reading and writing data
# library(readr) # to read CSV data
library(openxlsx)

source("support_files/credentials.R")

## todays date
today_dat <- as_date(today())
today_hour <- hour(now())

## standard columns for log
# uuid, type, names, value, index, relevant, checked_by, checked_date, comment

####----set global variables ----------
kobo_server_url<-"https://kobo.humanitarianresponse.info/"
kc_server_url<-"https://kc.humanitarianresponse.info/"


form_id <- used_form_id #change the id to your form's id
url<-paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
#returns the CSV content of the form

#supply url for the data
# rawdata<-GET(url)
#if form data is not accessible publicly, then username and password needs to be provided. 
#Use the following code to download the data.
u <- user_name
pw <- password
rawdata<-GET(url,authenticate(u,pw),progress())

print(paste0("Status Code: ",rawdata$status_code))

# to check the elements returned:
str(rawdata)

d_content <- content(rawdata,"raw",encoding="UTF-8")
d_content_csv <- read_csv(d_content)

d_content_csv <- as.data.frame(d_content_csv)

#using openxlsx package to save data
openxlsx::write.xlsx(d_content_csv, file =paste0(getwd(),"/inputs/kobo_data_downloaded_",today_dat,".xlsx"),sheetName="data", row.names = FALSE)
