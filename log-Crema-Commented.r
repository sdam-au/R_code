
## Load R packages ####
library(rcarbon)
library(oxcAAR)
library(magrittr)
library(dplyr)
library(readr)
#library(TTR)
#library(trapezoid)
#
## Source R functions ####
source("PATH-TO/src/outlierAnalysis.R")
#source("PATH-TO/src/utilities.R")
#source("PATH-TO/src/oxcalReadjs.R")
#source("PATH-TO/src/oxcalScriptCreator.R")
#source("PATH-TO/src/mcsim.R")



#### READ AND PREPARE DATA FOR CERAMIC PHASE MODELS ####
c14data = read.csv("PATH-TO/data/c14dates.csv")
colnames(c14data)
# [1] "LabCode"         "SampleID"        "CombineGroup"    "Region"          "SiteID"          "Site_Name_JP"    "Site_Address_JP"
# [8] "Context_JP"      "MaterialType_JP" "MaterialType_En" "PotteryPhase_JP" "Phase"           "Phase_Details"   "PhaseAnalysis"  
#[15] "CRA"             "Error"           "Delta13C"        "Delta13CError"   "Source"



### outlier analysis ###

# groups (?)
grps <- as.vector(stats::na.omit(unique(c14data$CombineGroup)))
# [1]  0  1  2  3  4  5  6  7 11 12 13 14 15 16 17 18 19 20 26 27 28 29 30 31 32 33 34 39 40 41 48 49 50 51 52 53  8  9 10 54 21 22 23 24 25 55 58
#[48] 59 60 46 47 42 43 44 61 62 63 64 65 66 67 69 70 35 36 37 38 71 72 73 74 75 76 77 78 79 45 68 56 57



# EXCLUDE OUTLIERS

# set oxcal path for Windows
setOxcalExecutablePath(path="PATH-TO/OxCal/bin/OxCalWin.exe")

# check the last one
tmp <- subset(c14data,CombineGroup==57)

outlierExcluder(c14=tmp$CRA, errors=tmp$Error, id=tmp$LabCode)
#OxCal v4.3.2 (c) Bronk Ramsey (2017)
#Oxcal is already installed correctly.
#OxCal v4.3.2 (c) Bronk Ramsey (2017)
#MCMC analysis
#Sort    Burn    Trial   Save    Shrink  kPasses Done    Ok      Convergence
#[.]     [.]     [.]     [.]             3.0     10.0    100.0   100.0
#[.]     [.]     [.]     [.]             6.0     20.0    100.0   58.9
#[.]     [.]     [.]     [.]             12.0    22.2    100.0   69.2
#[.]     [.]     [.]     [.]     [.]     24.0    22.2    100.0   80.0
#[.]     [.]     [.]     [.]             48.0    22.2    100.0   88.1
#[.]     [.]     [.]     [.]             96.0    22.2    100.0   96.0
#[.]     [.]     [.]     [.]             144.0   33.3    100.0   94.7
#[.]     [.]     [.]     [.]             240.0   26.3    100.0   97.8
#[.]     [.]     [.]     [.]             336.0   36.8    100.0   97.8
#[.]     [.]     [.]     [.]             432.0   47.4    100.0   97.7
#[.]     [.]     [.]     [.]             528.0   57.9    100.0   97.7
#[.]     [.]     [.]     [.]             624.0   68.4    100.0   97.7
#[.]     [.]     [.]     [.]             720.0   78.9    100.0   97.7
#[.]     [.]     [.]     [.]             816.0   89.5    100.0   97.7
#[.]     [.]     [.]     [.]             912.0   100.0   100.0   97.7
#[.]     [.]     [.]     [.]             1008.0  110.5   100.0   97.7
#$finaloutput
#$finaloutput$agreement
#[1] 98.6
#
#$finaloutput$pval
#[1] 0.5270893
#
#$finaloutput$df
#           id  c14 errors outlier.prob
#1 BETA-211838 4990     40         0.04
#2    MTC-6740 4940     70         0.03
#
#
#$df
#           id  c14 errors outlier.prob exclude
#1 BETA-211838 4990     40           NA   FALSE
#2    MTC-6740 4940     70           NA   FALSE


## CHECK OUTLIERS by groups
length(grps)
#[1] 80


# ouliers, aggreements, p-values, and data-frames
outl <- vector()
outll <- list()
aggr  <- list(); length(aggr )<-length(grps)
pval  <- list(); length(pval )<-length(grps)
outdf <- list(); length(outdf)<-length(grps)
#
k<-1 #counter
for (i in grps) { 
x <- subset(c14data,CombineGroup==i)
tmp <- outlierExcluder(c14=x$CRA, errors=x$Error, id=x$LabCode)
aggr[[k]] <- tmp$finaloutput$agreement
outdf[[k]] <- tmp$finaloutput$df
pval[[k]] <- tmp$finaloutput$pval
if(any(tmp$df[,ncol(tmp$df)]==TRUE)==TRUE) {
outll[[length(outll)+1]] <- tmp$df
outl<-append(outl,i)
} else { NA }
k<-k+1
};rm(i,k)
attr(outll,"names") <- outl


# which groups have outliers?
outl
#[1]  7 14 20 27  9 21 38 77


# last one as example
tmp <- subset(c14data,CombineGroup==77)
outlierExcluder(c14=tmp$CRA, errors=tmp$Error, id=tmp$LabCode)
#...
#$finaloutput
#$finaloutput$agreement
#[1] 98.3
#
#$finaloutput$pval
#[1] 0.3173105
#
#$finaloutput$df
#        id  c14 errors outlier.prob
#1 PLD-9720 6225     35         0.04
#2 PLD-9722 6270     30         0.04
#
#
#$df
#        id  c14 errors outlier.prob exclude
#1 PLD-9720 6225     35         0.04   FALSE
#2 PLD-9721 6150     30           NA    TRUE
#3 PLD-9722 6270     30         0.04   FALSE



# (from this part, code needs to be improved)

# to remove outliers

for (i in 1:length(grps))
{
  print(paste0(i," out of ",length(grps)))
  tmp=subset(c14data,CombineGroup==grps[i])
  tmp2=outlierExcluder(c14=tmp$CRA,errors=tmp$Error,id=tmp$LabCode)
  c14data$outlier[match(tmp2$df$id,c14data$LabCode)]=tmp2$df$exclude
}
#remove outliers
c14data=subset(c14data,!outlier)
#create R image file
save(c14data,file="./R_images/c14data.RData")

## Generate Summary Table ### (with Radiocarbon Dates for Bayesian Modelling) of ceramic? phases
phases <- c("S0","S1.1","S1.2","S2.1","S2.2",paste0("S",3:8),paste0("Z",1:7),"C1","C234","C56","C78",paste0("C",9:14),paste0("K",1:8),paste0("B",1:6))


# 
table1 = data.frame(phases,samples=NA,effsamples=NA,nsites=NA)

for (i in 1:length(phases))
{
  tmp = subset(c14data,PhaseAnalysis==phases[i])
  table1$samples[i] = nrow(tmp)
  table1$effsamples[i] = sum(is.na(tmp$CombineGroup)) + length(unique(tmp$CombineGroup[which(!is.na(tmp$CombineGroup))]))
  table1$nsites[i] = length(unique(tmp$SiteID))
}

head(table1)
#  phases samples effsamples nsites
#1     S0      16         16      4
#2   S1.1      64         62     16
#3   S1.2      77         77     24
#4   S2.1      94         90     20
#5   S2.2      38         38      8
#6     S3     107        107     47


# store base table (to be manually edited)
#write.csv(table1,file="./manuscript/tables/table1_base.csv",row.names=FALSE)


###  TBD   ###  

## Ceramic Phase Modelling (via OxCal) ####

## Generate OxCal Script ##

# TBD

