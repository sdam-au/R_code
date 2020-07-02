## Install packages
devtools::install_github("mplex/cedhar", subdir="pkg/sdam")
## Load packages
library(sdam)
library(jsonlite)
library(tidyverse)
library(getPass)

# Input your sciencedata.dk username - type directly into the RStudio console
user <- readline("your sciencedata username: ")

# Make the request (you will be asked for password in a new pop-up window)
resp = request("EDH_utf8_sample.json", path="/sharingin/648597@au.dk/SDAM_root/SDAM_data/EDH/", method="GET", cred=c(user, getPass("your sciencedata password: ")))

# Make a list from the request
list_json <- jsonlite::fromJSON(resp)

# transform to tibble
EDH_tibble = as_tibble(list_json)
head(EDH_tibble)

# subset the tibble only to inscription_type containing word 'votive'
votive_insc <- EDH_tibble %>% 
  filter(type_of_inscription==(str_subset(type_of_inscription, "votive")))
votive_insc

# transform the subset to json and save it
votive_insc_json <- toJSON(votive_insc)
write(votive_insc_json, file="data/EDH_votive_sample.json")

# save to Sciencedata into the sdam shared drive 
request("data/EDH_votive_sample.json", path="/sharingout/648597@au.dk/SDAM_root/SDAM_data/EDH/", 
        method="PUT", force=TRUE, cred=c(user, getPass("your sciencedata password: "))) 

# if you got Status 201 response in the console, the file should be now on Sciencedata.dk in the folder specified. 
# If you got Status 401 or 403, there is problem with your authentication to Sciencedata.dk. You may check 
# a) your password, 
# b) rerun the user <- readline("your sciencedata username: ")
# c) try with an array for credentials such as cred <- c("USERNAME", "PASSWORD")

# request(file, method=PUT, force=TRUE)