## Install packages
devtools::install_github("mplex/cedhar", subdir="pkg/sdam")
install.packages("rjson")
install.packages("tidyverse")
install.packages("getPass")
## Load packages
library(sdam)
library(rjson)
library(tidyverse)
library(getPass)

# Input your sciencedata.dk username - type directly into the RStudio console
user <- readline("your sciencedata username: ")

# Make the request (you will be asked for password in a new pop-up window)
resp = request("EDH_utf8.json", path="/sharingin/648597@au.dk/SDAM_root/SDAM_data/EDH/", method="GET", cred=c(user, getPass("your sciencedata password: ")))
# Make a list from the request
list_json <- fromJSON(resp)
# transform to tibble
EDH_tibble = as_tibble(list_json)
head(EDH_tibble)

#subset the tibble
Thracia <- EDH_tibble %>% 
  filter(province_label=="Thracia"| province_label=="Thracia?")
Thracia

#transform the subset to json
Thracia_json <- toJSON(Thracia)

# save to Sciencedata
request("Thracia_json", path="/sharingout/648597@au.dk/SDAM_root/SDAM_data/EDH/", method="PUT", cred=c(user, getPass("your sciencedata password: ")),
        subdomain = "silo4") 


