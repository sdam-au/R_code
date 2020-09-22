
## 
## FUNCTION clean() coerce missing data into NA and <NA>
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.1 (22-09-2020)
##
## Parameters
## x  a data frame 


clean <-
function (x) 
{
    nx <- x[, 1]
    classes <- vector()
    pos <- vector()
    if (any(class(x[, 1]) %in% c("POSIXt", "POSIXct")) == TRUE) {
        classes <- append(classes, "POSIXt")
        pos <- append(pos, 1)
    }
    else {
        ifelse(is.numeric(nx) == TRUE, classes <- append(classes, 
            "numeric"), classes <- append(classes, "character"))
    }
    for (k in seq(2, ncol(x))) {
        if (any(class(x[, k]) %in% c("POSIXt", "POSIXct")) == 
            FALSE) {
            ifelse(is.numeric(x[, k]) == TRUE, classes <- append(classes, 
                "numeric"), classes <- append(classes, "character"))
        }
        else {
            classes <- append(classes, "POSIXt")
            pos <- append(pos, k)
        }
        nx <- cbind(nx, x[, k])
    }
    rm(k)
    colnames(nx) <- colnames(x)
    nxdf <- as.data.frame(nx)
    classes[pos] <- "character"
    nxdf[] <- Map(`class<-`, nxdf, classes)
    nxdf <- as.data.frame(nxdf)
    nxdf <- sapply((nxdf), function(xx) ifelse(xx == "NULL", 
        NA, xx))
    nxdf <- as.data.frame(nxdf, stringsAsFactors = FALSE)
    classes[pos] <- "POSIXt"
    nxdf[] <- Map(`class<-`, nxdf, classes)
    nxdf <- as.data.frame(nxdf)
    nxdf
}
