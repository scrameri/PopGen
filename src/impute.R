impute <- function(df, fun = "mean", classes = NULL, summary = TRUE) {
  
  ## Author: sfcrameri@gmail.com, Apr 2022
  
  ## Check input ##
  stopifnot(inherits(df, "data.frame"),
            fun %in% c("mean", "zero", "class.mean"))
  if (fun == "class.mean") {
    stopifnot(!is.null(classes),
              length(classes) == nrow(df),
              inherits(classes, c("character","factor")))
    if (is.character(classes)) classes <- factor(classes)
  }
  
  ## Determine numeric columns
  on <- colnames(df)
  idx <- sapply(df, base::class) %in% c("numeric","integer")
  xmis <- df[,idx,drop=F]
  
  ## Get missingness summary ##
  missingness <- list()
  
  if (summary) {
    # dfmis <- apply(xmis, 2, function(x) {gsub("^NA$", NA, x)})
    
    # total missingness (displayed in summary(genind))
    # df.perc <- sum(is.na(dfmis)) / (nrow(dfmis)*ncol(dfmis))
    df.perc <- sum(is.na(xmis)) / (nrow(xmis)*ncol(xmis))
    missingness[["total"]] <- df.perc
    
    # missingness per variable
    # df.var.perc <- apply(dfmis, 2, function(x) {sum(is.na(x)) / length(x)})
    df.var.perc <- apply(xmis, 2, function(x) {sum(is.na(x)) / length(x)})
    missingness[["per.variable"]] <- df.var.perc
    
    # missingness per individual
    # df.ind.perc <- apply(dfmis, 1, function(x) {sum(is.na(x)) / length(x)})
    df.ind.perc <- apply(xmis, 1, function(x) {sum(is.na(x)) / length(x)})
    missingness[["per.individual"]] <- df.ind.perc
    
  }
  
  ## Define impute function ##
  .impute <- function(x, fun, ...) {
    
    # check input
    stopifnot(is.numeric(x))
    if (any(is.na(x))) {
      
      # impute missing data
      switch(fun,
             mean = {x[is.na(x)] <- mean(x, na.rm = TRUE)},
             zero = {x[is.na(x)] <- 0}
      )
    }
    
    # return imputed vector
    return(x) 
  }
  
  ## Impute missing numeric data ##
  switch(fun, 
         mean = {
           missingness[["imputed"]] <- data.frame(apply(xmis, MARGIN = 2, FUN = .impute, fun = "mean"))
         }, 
         zero = {
           missingness[["imputed"]] <- data.frame(apply(xmis, MARGIN = 2, FUN = .impute, fun = "zero"))
         },
         class.mean = {
           tab <- array(NA, dim = c(0, ncol(xmis)), dimnames = list(NULL, colnames(xmis)))
           
           for (i in levels(droplevels(classes))) {
             xmissub <- xmis[classes == i,]
             tmp <- apply(xmissub, MARGIN = 2, FUN = .impute, fun = "mean", simplify = T)
             if (!is.array(tmp)) tmp <- data.frame(t(tmp))
             rownames(tmp) <- rownames(xmissub)
             colnames(tmp) <- colnames(tab)
             tab <- rbind(tab, tmp)
           }
           stopifnot(all(rownames(tab) %in% rownames(xmis)))
           tab <- tab[rownames(xmis),]
           
           # if a class is entirely NA, use the global mean
           if (anyNA(tab)) tab <- data.frame(apply(tab, MARGIN = 2, FUN = .impute, fun = "mean"), check.names = F)
           
           missingness[["imputed"]] <- data.frame(df[,!idx,drop=F], tab, check.names = F)[,on]
         }
  )
  
  
  ## Get imputation info ##
  na <- which(apply(xmis, 2, anyNA))
  switch(fun, 
         mean = {
           mean.imp <- apply(xmis[,na], 2, mean, na.rm = TRUE)
           missingness[["imputation"]] <- unlist(mean.imp)
         },
         zero = {
           zero.imp <- rep(0, length(na))
           names(zero.imp) <- names(na)
           missingness[["imputation"]] <- zero.imp
         },
         class.mean = {
           class.mean.imp <- apply(xmis, 2, tapply, classes, mean, na.rm = TRUE)
           missingness[["imputation"]] <- class.mean.imp[,na]
         }
  )
  
  ## return results
  return(missingness)
}
