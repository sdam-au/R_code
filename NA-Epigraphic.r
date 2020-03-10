####  EPIGRAPHIC DATABASE HEIDELBERG: INSCRIPTIONS
####
####  NETWORK TIES: MEASURES OF SIMILARITY OF ARTEFACT ASSEMBLAGES AND GEOGRAPHIC PROXIMITY

# Prerequisites:
# install.packages("multigraph")
# install.packages("multiplex")
# install.packages("devtools")

##
library("devtools")
#source_url("https://raw.githubusercontent.com/mplex/cedhar/master/code/get.edh.r")
# magic code that detects similarity in column variables among observations
source_url("https://raw.githubusercontent.com/mplex/cedhar/master/code/simil.r")
###


#### load the data
load(file="data/epalln.Rdata")
####

ls()
head(epalln)

# CREATE EPIGRAPHIC NETWORK DATA LIST WITH VARIABLES MEASURES OF 
# SIMILARITY OF ARTEFACT ASSEMBLAGES AND GEOGRAPHIC PROXIMITY (E.G.

# with this function you select those artefact attributes you are interested in 
#comparing and create a dataframe \netl\ from them 
epinetl <- lapply(epalln, function (x) x[c("ID", "type_of_monument", "language", "material", 
                  "country", "findspot_ancient", "type_of_inscription", "not_after", "not_before", "province_label")] )



# EXECUTE A FUNCTION CALL ON THE LIST TO CONVERT LIST OF DATA INTO A DATA FRAME

epinet <- as.data.frame(do.call(rbind, epinetl))



# TAKE A LOOK AT IT
head(epinet, 8)

str(epinet$type_of_inscription)

# REMOVE QUESTION MARKS
epinet2 <- as.data.frame(do.call(rbind, lapply(epinetl, function (x)  as.list(gsub('\\?', '', x)) )) )
colnames(epinet2) <- names(epinetl[[1]])



# TAKE A LOOK AT IT again
head(epinet2, 8)

## COUNTRIES

unique(unlist(epinet2$country))


## ROMAN PROVINCES

unique(unlist(epinet2$province_label))

# INSCRIPTION TYPES
unique(unlist(epinet2$type_of_inscription))

# FREQUENCY
sort(table(unlist(epinet2$type_of_inscription)))
sort(table(unlist(epinet2$province_label)))

## e.g. EPIGRAPHIC MATERIAL IN "GREEK-LATIN" FROM EGYPT

subset(subset(epinet2, country=="Egypt"), language=="Greek-Latin")
subset(subset(epinet2, country=="Egypt"), language=="Latin")
subset(subset(epinet2, country=="Egypt"), language=="Greek")

## COMPUTE SIMILARITY AMONG EGYPTIAN EPIGRAPHS WITH FUNCTION simil()
## CASES: 'type_of_monument' (2), 'material' (4), AND 'findspot_ancient' (6), 'type_of_inscription' (7)

epEgs <- simil(subset(epinet2, country=="Egypt"), c(2,4,6))
epEgs267 <- simil(subset(epinet2, country=="Egypt"), c(2,6,7))
epEgs2 <- simil(subset(epinet2, country=="Egypt"), c(2))
epEgs6 <- simil(subset(epinet2, country=="Egypt"), c(6))
epEgs7 <- simil(subset(epinet2, country=="Egypt"), c(7))


## HOW MANY INSCRIPTIONS?

dim(epEgs)
dim(epEgs7)
dim(epEgs267)

## CALCULATE FREQUENCY OF DIFFERENT KINDS OF INSCRIPTIONS
data.frame(sort(table(unlist(epinet2$type_of_inscription)), 
                  decreasing = TRUE))

## PLOT GRAPH OF THESE SIMILARITIES WITH CUSTOMIZED LAYOUT AND NO ISOLATES

## PLOT GRAPH OF EGYPTIAN INSCRIPTIONS SIMILARITIES WITH CUSTOMIZED LAYOUT AND NO ISOLATES

# install.packages("multigraph")
library("multigraph")

scp <- list(directed=FALSE, valued=TRUE, ecol=8, pos=0, cex=2.5, vcol="blue")

multigraph(rm.isol(epEgs7), layout="force", maxiter=14, scope=scp,
           main="Similarity of inscriptions found in Egypt by inscription type", values = TRUE)

scp <- list(directed=FALSE, valued=TRUE, ecol=8, pos=0, cex=2.5, vcol="yellow")

multigraph(rm.isol(epEgs), layout="force", maxiter=5, seed=1, scope=scp, main="Graph similarity of inscriptions found in Egypt (by: type_of_monument, material, findspot_ancient)")



## LOOK SIMILARITY OF EGYPTIAN INSCRIPTIONS AT EACH CASE SEPARATELY

# type_of_monument
epEgs2 <- simil(subset(epinet2, country=="Egypt"), 2)

# material
epEgs4 <- simil(subset(epinet2, country=="Egypt"), 4)

#findspot_ancient
epEgs6 <- simil(subset(epinet2, country=="Egypt"), 6)



## BIND EGYPTIAN INSCRIPTIONS SIMILARITIES INTO A 3D ARRAY (and remove isolates)

epEGs <- rm.isol(zbind(epEgs2, epEgs4, epEgs6))


## PLOT MULTIGRAPH OF EGYPTIAN INSCRIPTIONS SIMILARITIES WITH CUSTOMIZED LAYOUT AND NO ISOLATES

multigraph(epEGs, layout="force", maxiter=5, seed=1, scope=scp, main="Multigraph similarities of Egyptian epigraphs, by type of inscription, material and findspot ancient")




## LOOK AT THE DIFFERENT COMPONENTS IN EGYPTIAN INSCRIPTIONS AS A MULTIPLEX NETWORK

epEGscmp <- comps(epEGs)
epEGscmp

## SELECT THE FIRST AND FOURTH COMPONENT OF EGYPTIAN INSCRIPTIONS

epEGscmp1 <- rel.sys(epEGs, type="toarray", sel=epEGscmp$com[[1]])
epEGscmp1
epEGscmp4 <- rel.sys(epEGs, type="toarray", sel=epEGscmp$com[[4]])
epEGscmp4

## NOW PLOT THESS SINGLE COMPONENTS OF EGYPTIAN INSCRIPTIONS

multigraph(epEGscmp1, layout="force", maxiter=50, seed=1, scope=scp, main="Multigraph similarities of inscriptions found in Egypt by type of monument")

multigraph(epEGscmp4, layout="force", maxiter=50, seed=1, scope=scp, main="Multigraph similarities of inscriptions found in Egypt by ancient findspot")

# how can we tell the category of each component? I was manually searching for the numbers and comparing what it might be, but I am sure ther eis a better way

# Can we color the nodes according to inscription type?

