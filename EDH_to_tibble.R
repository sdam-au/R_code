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
# Input your sciencedata.dk
user <- readline("your sciencedata username: ")

# Make the request (you will be asked for password)
resp = request("EDH_utf8.json", path="/sharingin/648597@au.dk/SDAM_root/SDAM_data/EDH/", method="GET", cred=c(user, getPass("your sciencedata password: ")))
# Make a list from the request
list_json <- fromJSON(resp)
# 
EDH_tibble = as_tibble(list_json)
head(EDH_tibble)
