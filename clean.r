
## 
## FUNCTION clean() coerce missing data into NA and <NA>
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.2 (24-09-2020)
##
## Parameters
## x  a data frame 


clean <-
function (x) 
{
    ifelse(any(class(x) %in% c("tbl_df", "tbl")) == TRUE, x <- as.data.frame(x), 
        NA)
    x[x == "list()"] <- NA
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
    flglst <- FALSE
    for (k in seq(2, ncol(x))) {
        if (is.list(x[, k]) == TRUE && isTRUE(length(unlist(x[, 
            k])) > length(x[, k])) == TRUE) {
            flglst <- TRUE
            dm <- floor(length(unlist(x[, k]))/length(x[, k]))
            lst <- unlist(x[, k])
            classes <- append(classes, rep(class(x[, k][[1]]), 
                dm))
            clnms <- colnames(x)[seq_len(k - 1L)]
            clnms <- append(clnms, paste(colnames(x)[k], seq_len(dm), 
                sep = ""))
            ifelse(isTRUE(k >= dim(x)[2]) == TRUE, NA, clnms <- append(clnms, 
                colnames(x)[seq(k + 1L, dim(x)[2])]))
            for (i in seq_len(dm)) {
                nx <- cbind(nx, lst[seq(i, length(lst), dm)])
            }
            rm(i)
        }
        else {
            if (any(class(x[, k]) %in% c("POSIXt", "POSIXct")) == 
                FALSE) {
                if (is.numeric(x[, k]) == TRUE) {
                  classes <- append(classes, "numeric")
                }
                else {
                  classes <- append(classes, "character")
                }
            }
            else {
                classes <- append(classes, "POSIXt")
                pos <- append(pos, k)
            }
            nx <- cbind(nx, x[, k])
        }
    }
    rm(k)
    if (isTRUE(flglst == FALSE) == TRUE) {
        colnames(nx) <- colnames(x)
    }
    else {
        colnames(nx) <- clnms
    }
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
