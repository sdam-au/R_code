
# script by Antonio (10-06-2020)


# Bulgarian mound burials attribute data (Adela)
mba = read.csv("PATH-TO/data/BurialAtts.csv", sep=";", stringsAsFactors = FALSE)
#'data.frame':   3063 obs. of  32 variables:
# $ X0pp0                  : int  1002 1002 1002 1002 1003 1003 1004 1004 1004 1005 ...
# $ Name                   : chr  "Mound 5" "Mound 5" "Mound 5" "Mound 5" ...
# $ GraveNo                : chr  "1" "1" "2" "2" ...
# $ Enclosure.Type         : chr  "Simple pit" "Simple pit" "Simple pit" "Simple pit" ...
# $ LaborAssessment        : chr  "no elaboration (eg.a pit or no indistinct enclosure in PH or LIA period)" "no elaboration (eg.a pit or no indistinct enclosure in PH or LIA period)" "no elaboration (eg.a pit or no indistinct enclosure in PH or LIA period)" "no elaboration (eg.a pit or no indistinct enclosure in PH or LIA period)" ...
# $ Burial.No              : chr  "1" "2" "1" "2" ...
# $ Extra.skeletal.remains.: chr  "N" "N" "N" "N" ...
# $ Notes.on.burial        : chr  "" "" "" "" ...
# $ Assemblage.            : chr  "N" "N" "N" "N" ...
# $ AnimalRemains          : chr  "" "" "" "" ...
# $ Lithics                : chr  "" "" "" "" ...
# $ HMPottery              : chr  "" "" "" "" ...
# $ Storage                : chr  "" "" "" "" ...
# $ FineVessels            : chr  "" "" "" "" ...
# $ DrinkingCups           : chr  "" "" "" "" ...
# $ Jewellery              : chr  "" "" "" "" ...
# $ Weapons                : chr  "" "" "" "" ...
# $ SpecialFinds           : chr  "" "" "" "" ...
# $ Imports                : chr  "" "" "" "" ...
# $ Grave.Rank.symbols     : chr  "0 - No symbols of status present" "0 - No symbols of status present" "0 - No symbols of status present" "0 - No symbols of status present" ...
# $ StartDate              : chr  "-3500" "-3500" "-3500" "-3500" ...
# $ Enddate                : chr  "-3000" "-3000" "-3000" "-3000" ...
# $ Chronology.rating      : chr  "1 - rough estimate by author on basis of the assemblage" "1 - rough estimate by author on basis of the assemblage" "1 - rough estimate by author on basis of the assemblage" "1 - rough estimate by author on basis of the assemblage" ...
# $ X                      : logi  NA NA NA NA NA NA ...
# $ X.1                    : logi  NA NA NA NA NA NA ...
# $ X.2                    : logi  NA NA NA NA NA NA ...
# $ X.3                    : logi  NA NA NA NA NA NA ...
# $ X.4                    : logi  NA NA NA NA NA NA ...
# $ X.5                    : logi  NA NA NA NA NA NA ...
# $ X.6                    : logi  NA NA NA NA NA NA ...
# $ X.7                    : logi  NA NA NA NA NA NA ...
# $ X.8                    : logi  NA NA NA NA NA NA ...

nrow(mba)
#[1] 3063


# remove missing information
strDate <- stats::na.omit(mba$StartDate)
endDate <- stats::na.omit(mba$Enddate)
#
strDate <- strDate[-which(strDate=="")]
endDate <- endDate[-which(endDate=="")]
#
strDate <- strDate[-which(endDate=="no data")]
endDate <- endDate[-which(endDate=="no data")]
#
strDate <- strDate[-which(endDate=="?")]
endDate <- endDate[-which(endDate=="?")]



# plot cleaned data

years <- c(min(as.numeric(strDate)),max(as.numeric(endDate)))
#[1] -3500  1900
n <- length(strDate)
#
plot(strDate, seq_len(n), pch=20, col="#C0C0C0", xlab="Year", ylab="ID", xlim=years, main="Bulgarian Burial Mounds Start and End Dates (cleaned)")
points(endDate, seq_len(n), pch=20, col="#808080")
segments(as.numeric(strDate), seq_len(n), as.numeric(endDate), seq_len(n), col=grDevices::adjustcolor(8,alpha=.8))



# probability distribution of burial mounds with unique dates
bmd <- unique(cbind(as.numeric(strDate), as.numeric(endDate)))

head(bmd)
#         x     y
#[1,] -3500 -3000
#[2,]   150   200
#[3,]   200   225
#[4,]   200   250
#[5,]   200   400
#[6,]   100   150


# compute artihmetic mean between start and end dates
bmdm <- apply((bmd), 1, mean)

# histogram with the probability densities
hist(bmdm, breaks=50, prob=TRUE, main="Burial mounds histogram with mean interval data") #, xlab="years")
# add a normal distributed curve and smoothing line
curve(dnorm(x, mean=mean(bmdm), sd=sd(bmdm)), add=TRUE, col="darkblue", lwd=2) 
lines(density(bmdm, from=-3500), col="red", lwd=2)


# which distribution fits best the data (with a plot)
require("propagate")

fitdistr(bmdm)
#
#...
#
#Best fit is Johnson SU Distribution.
#Parameters:
#         xi      lambda       gamma       delta 
#319.4374996 105.7387337   1.3594608   0.5535048 
#Standard errors:
#         xi      lambda       gamma       delta 
#33.57684733 24.85410857  0.26956780  0.07531578 
#Goodness of fit:
#BIC = -1522.493>

# model selection from the Bayesian information criterion



## Hypothesis testing

# TBD




