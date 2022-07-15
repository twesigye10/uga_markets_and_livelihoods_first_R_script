# devtools::install_github("zackarno/koboAPI")

## load packages
library(koboAPI)

source("support_files/credentials.R")


u <- user_name
pw <- password
forms <- download_forms_all(user=u, pwd =pw )
forms
# dataset
form_id <- used_form_id
dat <- download_data(form_id, user=u, pwd = pw)

# tool itself
tool <- download_form(form_id, user=u, pwd = pw)

# create kobold object
kbo <- kobold::kobold(survey = tool$survey, choices = tool$choices, data = dat)
kbo_labelled <- kobold::choices = choice_names_to_labels(object = kbo)
