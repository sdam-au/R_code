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

# Make the request (you will be asked for password in a new pop-up window), 
resp = request("insertnameofyourfile.extension", path="/sharingin/648597@au.dk/SDAM_root/SDAM_data/EDH/", method="GET", cred=c(user, getPass("your sciencedata password: ")))

newfile <- resp

# Work with your file in R as you are used to

# write the file into your local computer and name it

write(newfile, file="newfile.extension")

# save to Sciencedata into the sdam shared drive 
request("newfile.extension", path="/sharingout/648597@au.dk/SDAM_root/SDAM_data/EDH/", 
        method="PUT", cred=c(user, getPass("your sciencedata password: "))) 

# if you got Status 201 response in the console, the file should be now on Sciencedata.dk in the folder specified. 
# If you got Status 401 or 403, there is problem with your authentication to Sciencedata.dk. You may check 
# a) your password, 
# b) rerun the user <- readline("your sciencedata username: ")
# c) try with an array for credentials such as cred <- c("USERNAME", "PASSWORD")

