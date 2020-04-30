####  Loading Python outputs into R from Antonio (loading as list)

## install packages if you don't have them 
# devtools::install_github("sdam-au/sdam")
# install.packages("rjson")


## First we connect to sddk and load data from it into R

# load library
library(sdam)
library(tidyverse)

edh_utf8sm <- request("EDH_utf8_sample.json", path="/sharingin/648597@au.dk/SDAM_root/SDAM_data/EDH/", method="GET", cred=c("yourusername", "yourpassword"))
edh_utf8rich <- request("EDH_inscriptions_rich.json", path="/sharingin/648597@au.dk/SDAM_root/SDAM_data/EDH/", method="GET", cred=c("yourusername", "yourpassword"))

## Second, we convert the json into a dataframe to be more manageable in R
require("rjson")  # https://cran.r-project.org/package=rjson

edhvjson <- fromJSON(edh_utf8sm)
is(edhvjson)
#[1] "list"   "vector"
glimpse(edhvjson)

length(edhvjson)
#[1] 40
# each of the 40 entries is a List of 1000 items in case of edh sample in EDH_utf8_sample.json

length(edhvjson[[1]])
length(edhvjson[[40]])
#[1] 1000
# these are just examples of 'nulls'
tail(edhvjson[[15]])
tail(edhvjson[[30]])
tail(edhvjson[[40]])

# So to get the texts you might want you can subset
head(edhvjson[[13]])
