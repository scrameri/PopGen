# main function
split.data <- function(X, fac, group = NULL, by = NULL, SplitRatio = 2/3, nmin = 1,
                       drop = NULL, verbose = TRUE) {
  
  ## sfcrameri@gmail.com, Jul. 2022
  
  ## USAGE
  # X           data.frame      complete dataset to be split by rows
  # fac         factor          grouping factor of X (can also be a character string, or a column name of X)
  # group       character       (optional) column name of X denoting a grouping variable. Rows of the same grouping variable cannot be split into different sets. Often, this variable contains an identifyer for individuals, in cases where there are multiple samples per individual. If NULL, rows will be split randomly into a training and test set.
  # by          character       (optional) column name of X denoting stratification variable. Rows of the same stratification variable tend to be split into different sets
  # SplitRatio  numeric         desired ratio of samples in training set
  # nmin        numeric         minimum number of rows per grouping factor level (if group=NULL) or per group (if group!=NULL) in order to keep that grouping factor level in training and test sets
  # drop        character       character string denoting unwanted classes (levels of <fac>), which will not appear in the training or test sets but which will be retained in the new set (see value)
  # verbose     logical         if TRUE, will print summary messages
  
  # check
  if (is.character(fac) & length(fac) == 1) {
    stopifnot(fac %in% colnames(X))
    fac <- X[,fac]
  }
  if (is.character(fac)) {
    fac <- as.factor(fac)
  }
  stopifnot(inherits(X, c("data.frame","matrix")),
            is.factor(fac), length(fac) == nrow(X),
            is.numeric(SplitRatio), SplitRatio >= 0, SplitRatio <= 1,
            is.numeric(nmin), nmin > 0)
  if (!is.null(group)) {
    stopifnot(is.character(group),
              group %in% colnames(X))
  }
  if (!is.null(by)) {
    stopifnot(is.character(by),
              by %in% colnames(X))
    X[,by] <- factor(X[,by])
  }
  if (!is.null(drop)) {
    stopifnot(is.character(drop))
    if (!all(drop %in% levels(fac))) warning("These class(es) in <drop> are not in levels of <fac>:\n", paste(drop[!drop %in% levels(fac)], collapse = ","))
  }
  
  # remove small classes (n < nmin) or classes to be dropped
  if (!is.null(group)) {
    t <- sort(apply(table(fac, X[,group]), 1, function(x) {sum(x>0)}), decreasing = TRUE)
    # names(t[t < nmin])
  } else {
    t <- table(fac)
    # names(t[t < nmin])
  }
  fac2rm <- unique(c(names(t[t < nmin]), drop))
  X2 <- X[!fac %in% fac2rm,]
  fac2 <- droplevels(fac[!fac %in% fac2rm])
  dnew1 <- X[fac %in% fac2rm,]
  Ynew1 <- fac[fac %in% fac2rm]
  
  # split
  # sample = sample.split(Y = fac2, group = X2[,group], SplitRatio = SplitRatio) # caTools way
  sample = logical(length = nrow(X2))
  for (class in levels(fac2)) {
    
    # subset X by kept classes
    Xcl <- X2[fac2 %in% class,,drop = FALSE]
    
    # tabulate <group> and <by> factors
    if (!is.null(by)) {
      if (!is.null(group)) {
        t <- apply(table(Xcl[,group], droplevels(Xcl[,by])), 2, as.numeric)
        rownames(t) <- names(table(Xcl[,group]))
      } else {
        # t <- matrix(table(rownames(Xcl), Xcl[,by]), nrow = nrow(Xcl))
        t <- apply(as.matrix(table(rownames(Xcl), Xcl[,by])), 2, as.numeric)
        rownames(t) <- rownames(Xcl)
      }
    } else {
      if (!is.null(group)) {
        t <- apply(table(Xcl[,group], rep(1, nrow(Xcl))), 2, as.numeric)
        rownames(t) <- names(table(Xcl[,group]))
      } else {
        t <- apply(table(rownames(Xcl), rep(1, nrow(Xcl))), 2, as.numeric)
        rownames(t) <- rownames(Xcl)
      }
    }
    
    # select most frequent <group> per <by> factor, add to training set proportionally to SplitRatio
    i <- apply(t, 2, function(x) {
      xord <- x[order(x, decreasing = T)]
      xmax <- xord[xord == max(xord)]
      x1 <- sample(names(xmax), 1)
      
      xrest <- x[!names(x) %in% x1]
      ct <- xrest[xrest>0]
      
      r <- (SplitRatio*length(x[x>0])) - 1
      x2 <- names(ct[sample(seq(length(ct)), round(r))])
      
      c(x1, x2)
    })
    
    # remove some training samples if the effective proportion is too high
    sel <- as.vector(unlist(i))
    
    if (length(sel) > round(nrow(t) * SplitRatio)) {
      # cat(class)
      d <- names(which.min(rowSums(t[i[[which.max(lengths(i))]],,drop=F])))
      sel <- sel[!sel %in% d]
    }
    
    # update sample vector
    if (!is.null(group)) {
      w <- Xcl[,group] %in% sel
    } else {
      w <- rownames(Xcl) %in% sel
    }
    sample[fac2 %in% class] <- w
  } 
  
  # create training and validation sets
  dtrain = X2[sample,]
  dtest  = X2[!sample,]
  
  Ytrain <- droplevels(fac2[sample])
  Ytest <- droplevels(fac2[!sample])
  
  # # correct training and validation sets (only needed if caTools::sample.split() is used)
  # train = X2[sample,]
  # test  = X2[!sample,]
  # switch(method,
  #        "test2train" = {
  #          dtest <- test[!test[,group] %in% train[,group],]
  #          dtrain <- X2[!rownames(X2) %in% rownames(dtest),]
  # 
  #          Ytest <- droplevels(fac2[!sample][!test[,group] %in% train[,group]])
  #          Ytrain <- droplevels(fac2[!rownames(X2) %in% rownames(dtest)])
  #        },
  #        "train2test" = {
  #          dtrain <- train[!train[,group] %in% test[,group],]
  #          dtest <- X2[!rownames(X2) %in% rownames(dtrain),]
  # 
  #          Ytrain <- droplevels(fac2[sample][!train[,group] %in% test[,group]])
  #          Ytest <- droplevels(fac2[!rownames(X2) %in% rownames(dtrain)])
  #        })
  
  # create new set (not in training or validation set)
  dnew <- dtest[!Ytest %in% Ytrain,]
  Ynew <- factor(Ytest[!Ytest %in% Ytrain], levels = levels(fac))
  
  dtest <- dtest[Ytest %in% Ytrain,]
  Ytest <- droplevels(Ytest[Ytest %in% Ytrain])
  
  dnew <- rbind(dnew1, dnew)
  Ynew <- factor(c(as.character(Ynew1), as.character(Ynew)))
  
  # check
  if (!is.null(group)) {
    stopifnot(!any(dtrain[,group] %in% dtest[,group]))
  }
  stopifnot(nrow(dtrain) == length(Ytrain),
            nrow(dtest) == length(Ytest),
            all(Ytrain %in% Ytest),
            all.equal(levels(Ytrain), levels(Ytest)))
  
  # make summary
  if ("fac" %in% colnames(X)) {
    dtrain[,"fac.replaced"] <- dtrain[,"fac"]
    dtest[,"fac.replaced"] <- dtest[,"fac"]
    dnew[,"fac.replaced"] <- dnew[,"fac"]
  }
  dtrain$fac <- Ytrain
  dtest$fac <- Ytest
  dnew$fac <- Ynew
  dp <- data.frame(ntrain = as.numeric(table(Ytrain)),
                   ntest = as.numeric(table(Ytest)),
                   grouptrain = as.numeric(sapply(levels(Ytrain), function(x) {length(unique(dtrain[dtrain[,"fac"] %in% x,group]))})),
                   grouptest = as.numeric(sapply(levels(Ytest), function(x) {length(unique(dtest[dtest[,"fac"] %in% x,group]))})),
                   ptrain = as.matrix(table(Ytrain) / table(droplevels(fac2[fac2 %in% Ytrain]))),
                   ptest = as.matrix(table(Ytest) / table(droplevels(fac2[fac2 %in% Ytrain]))))
  
  # be verbose
  if (verbose) {
    message("Training set: ", nrow(dtrain), "; Test set: ", nrow(dtest), "; New set: ", nrow(dnew))
    message("\nDesired SplitRatio: ", round(SplitRatio, 2), " ; Effective SplitRatio: ", round(nrow(dtrain)/(nrow(dtrain)+nrow(dtest)), 2), " (", paste(round(range(dp$ptrain), 2), collapse = " - "), ")")
    message("\nDropped these ", length(table(Ynew)), " / ", length(table(fac)),
            " classes from training and test set:\n", 
            paste(sort(names(table(Ynew))), collapse = ","))
    message("\nKept these ", length(table(Ytrain)), " / ", length(table(fac)),
            " classes in training and test set:\n",
            paste(sort(names(table(Ytrain))), collapse = ","))
  }
  
  # return
  args <- list(group = group, by = by, SplitRatio = SplitRatio, nmin = nmin, drop = drop)
  invisible(list(Xtrain = dtrain, Xtest = dtest, Xnew = dnew,
                 Ytrain = Ytrain, Ytest = Ytest, Ynew = Ynew,
                 args = args, summary = dp))
}
