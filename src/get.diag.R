get.diag <- function(df, fac, level = nlevels(fac)-1, verbose = TRUE, quantitative = FALSE,
                     min.tpr = 0.5, thresh = NULL, max.thresholds = 1000) {
  
  ## Usage:
  # df      data.frame
  # fac     grouping factor (same length as nrow(df))
  # level   grouping level. 1 only searches diagnostic features in a single group, nlevels(fac)-1 [default] considers all possible combinations of grouping factor levels.
  # verbose whether to be verbose during compuations [default: TRUE]
  # quantitative if TRUE, all variable are assumed to be quantitative (rather than a mix of quantitative and binary dummy variables)
  # min.tpr minimum true positive rate.
  # thresh  set a unique threshold (useful for genetic allele tables: set thresh = 2 for fixed homozygote)
  # max.thresholds if there are more than <max.thresholds> unique values in a quantitative variable, <max.thresholds> thresholds are computed and used.
  
  ## Value:
  # data.frame with the following components
  # FACTOR grouping factor (e.g. species)
  # NLEVELS number of grouping factor levels (e.g. number of species)
  # LEVEL grouping level (equal to the length of GROUP)
  # GROUP group (e.g. single species, or group of species) for which a VARIABLE is diagnostic
  # VARIABLE diagnostic character (state)
  # THR threshold (1 for qualitative character states, any number for quantitative characters)
  # STATE whether the GROUP has frequently a high (≥) or low (<) value for a VARIABLE
  # FREQ frequency of character state VARIABLE in GROUP
  # MISS percentage of missing data in VARIABLE
  # TPR true positive rate (proportion of individuals of a given group showing a given character state)
  # FPR false positive rate (proportion of individuals excluding a given group showing that character state)
  
  ## Author: simon.crameri@usys.ethz.ch, Jan 2022
  
  # check input
  if (!is.factor(fac)) fac <- factor(fac)
  if (level >= nlevels(fac)) level <- nlevels(fac)-1
  stopifnot(inherits(df, c("matrix","data.frame")),
            all.equal(length(fac), nrow(df)),
            is.numeric(level), level >=1)
  
  # search qualitative and quantitative variables
  if (quantitative) {
    v.qual <- character()
    v.quant <- names(df)
  } else {
    bin <- suppressWarnings(sapply(names(df), function(x) is.logical(df[,x]) | (is.numeric(df[,x]) & all(na.omit(df[,x]) <=1) & all(na.omit(df[,x]) >= 0)) | (is.factor(df[,x]) & length(unique(x)) == 2)))
    v.qual <- names(bin[bin])
    v.quant <- names(bin[!bin])
  }
  
  # be verbose
  fname <- gsub("__", "_", gsub(" ", "_", gsub('^factor|[()]|[]]|[[]|"|`', "", sub("(.*\\\\)(.*)", "(\\2)", sub("(.*\\$)(.*)", "(\\2)", gsub(",", "_", deparse(substitute(fac))))))))
  if (verbose) {
    message("Searching diagnostic characters to distinguish ", fname, "\n  ", length(v.qual), " qualitative and ", length(v.quant), " quantitative variables at level ", level, ".")
  }
  
  # loop over variables using different frequency thresholds
  res <- data.frame(FACTOR = fname, NLEVELS = nlevels(fac), LEVEL = NA, GROUP = NA,
                    VARIABLE = NA, THR = NA, STATE = NA, FREQ = NA, MISS = NA, TPR = NA, FPR = NA)[0,]
  
  # loop over variables, value thresholds (quantitative only) and frequency thresholds
  for (var in c(v.qual, v.quant))  {
    
    v <- df[,var]
    
    # check
    if (is.factor(v)) v <- as.numeric(v)
    stopifnot(!all(is.na(v)), is.numeric(v))
    if (var %in% v.qual) stopifnot(na.omit(v) <= 1, na.omit(v >= 0))
    
    # set thresholds for quantitative variables
    if (is.null(thresh)) {
      if (var %in% v.quant) {
        r <- range(v[!v %in% min(v, na.rm = T)], na.rm = T)
        thrs.range <- seq(r[1], r[2], length.out = max.thresholds) # if more than 1000 unique values, use range
        thrs.uniq <- sort(unique(v))[-1]
        thrs <- list(thrs.range, thrs.uniq)[[which.min(c(length(thrs.range), thrs.uniq))]]
      } else {
        thrs <- 1 
      }
    } else {
      thrs <- thresh
    }
    
    # loop over value thresholds (quantitative only) and frequency thresholds 
    for (thr in thrs) {
      
      # tabulate
      if (var %in% v.qual) {
        t <- table(Group = fac, Variable = v)
      } else {
        t <- table(Group = fac, Variable = factor(v >= thr, levels = c(FALSE, TRUE)))
        colnames(t) <- c("0","1")
      }
      
      # skip if variable is constant
      if (!all(c("0","1") %in% colnames(t))) {
        # if (verbose) cat("skipping invariant", var, "\r")
        next()
      } else {
        # if (verbose) cat(var,"\r")
      }
      
      for (freq in seq(1, min.tpr, by = -0.01)) {
        
        # high frequency or always present in
        high <- apply(t, 1, function(x) {(x[length(x)]/sum(x)) >= freq})
        
        # low frequency or always absent in
        low <- apply(t, 1, function(x) {(x[1]/sum(x)) >= freq})
        
        l <- length(low)
        shigh <- sum(high, na.rm = TRUE)
        slow <- sum(low, na.rm = TRUE)
        
        fhigh <- shigh/l
        flow <- slow/l
        
        # test for diagnosticity at level 1 (single group), 2 (two groups), 3 (three groups), etc.
        for (lev in seq(level)) {
          lowlev <- flow > 0 & flow <= lev/l
          highlev <- fhigh > 0 & fhigh <= lev/l
          assign(x = paste0("low", lev), value = lowlev)
          assign(x = paste0("high", lev), value = highlev)
          rm(list = c("lev","lowlev","highlev"))
        }
        
        # calculate TPR and FPR
        f <- fac[!is.na(v)]
        m <- sum(is.na(v))/nrow(df)
        
        clow <- names(low[which(low)])
        tpr.low <- sum(t[levels(fac) %in% clow,"0"])/sum(f %in% clow)
        fpr.low <- sum(t[!levels(fac) %in% clow,"0"])/sum(!f %in% clow)
        
        chigh <- names(high[which(high)])
        tpr.high <- sum(t[levels(fac) %in% chigh,"1"])/sum(f %in% chigh)
        fpr.high <- sum(t[!levels(fac) %in% chigh,"1"])/sum(!f %in% chigh)
        
        # save result if diagnostic at level <level>
        if (get(paste0("low", level))) {
          res.low <- data.frame(FACTOR = fname, NLEVELS = nlevels(fac), LEVEL = length(clow),
                                GROUP = paste(clow, collapse = "+"),
                                VARIABLE = var, THR = thr, STATE = "<", FREQ = freq, MISS = m,
                                TPR = tpr.low, FPR = fpr.low)
          res <- rbind(res, res.low)
        }
        if (get(paste0("high", level))) {
          res.high <- data.frame(FACTOR = fname, NLEVELS = nlevels(fac), LEVEL = length(chigh), 
                                 GROUP = paste(chigh, collapse = "+"),
                                 VARIABLE = var, THR = thr, STATE = "≥", FREQ = freq, MISS = m,
                                 TPR = tpr.high, FPR = fpr.high)
          res <- rbind(res, res.high)
        }
      }
    }
  }
  
  # order results
  res <- res[order(res$FACTOR, res$VARIABLE, res$GROUP, -res$TPR, res$FPR),]
  
  # filter reduntant results
  res <- res[!duplicated(as.matrix(res[,!names(res) %in% "FREQ"])),]
  res <- res[!duplicated(as.matrix(res[,!names(res) %in% c("THR","FREQ","TPR","FPR")])),]
  
  # be verbose
  if (verbose) {
    ndiag <- length(unique(subset(res, TPR >= 0.9 & FPR <= 0.1)$VARIABLE))
    ndiaggrp <- length(unique(subset(res, TPR == 1 & FPR == 0 & LEVEL == 1)$VARIABLE))
    message("  found ", ndiag, " potentially useful (TPR >= 90%, FPR <= 10%) variables,\n  of which ", ndiaggrp, " are diagnostic (TPR = 100%, FPR = 0%) to distinguish single groups!")
  }
  
  # return results
  rownames(res) <- NULL
  res$THR[res$VARIABLE %in% v.qual] <- NA
  res$STATE[res$VARIABLE %in% v.qual] <- ifelse(res$STATE[res$VARIABLE %in% v.qual] == "<", "absent", "present")
  return(res)
}
