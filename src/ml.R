#####################
### Preprocessing ###
#####################

# split data by factor and group
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
      # print(length(names(xmax))) # test
      x1 <- sample(names(xmax), 1) ###
      
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



##################
### tidymodels ###
##################

tidytune <- function(Xtrain, Ytrain, Xtest, Ytest, workflow, tuneGrid, folds,
                     trControl, metrics, metric.best = "accuracy",
                     seed.train = NULL, seed.final = NULL,
                     plot.tune = TRUE, plot.roc = FALSE, ...) {
  
  # author: Simon Crameri, sfcrameri@gmail.com, Aug 2022
  
  ## Description
  # 1) Perform hyperparameter tuning on training data (Xtrain) for any classification model specified as a tidymodels workflow::workflow(), with specified tuneGrid and cross-validation folds, trControl, and model performance metrics.
  # 2) Fit a classification model based on the full training data (Xtrain), and calculate honest prediction 
  # 3) if plot.tune=T Visualize the hyperparameter space and the corresponding classification performance
  
  ## Arguments
  # Xtrain      data.frame object with training set data (will be further split by kfold cross-validation for model tuning)
  # Ytrain      factor     classes of training samples
  # Xtest       data.frame object with test set data (for honest model performance assessment)
  # Ytest       factor     classes of test samples
  # workflow    workflow object with the recipe for inputs, roles, and operations (preprocessing) performed before model fitting, e.g. workflows::workflow(preprocessor = rec1 <- recipe(formula = class ~ ., data = data) %>% step_dummy(all_nominal_predictors()), spec = parsnip::discrim_linear(mode = "classification", ) %>% set_engine(engine = "MASS"))
  # tuneGrid    data.frame object specifying the hyperparameter search space, one column per tuned hyperparameter, hyperparameter combinations in rows
  # folds       vfold_cv object specifying kfold cross-validation splits, e.g rsample::vfold_cv(rec$template, v = 10, repeats = 1, strate = "class")
  # trControl   control_grid object listing control aspects for tuning, passed to tune::tune_grid(), e.g. tune::control_grid(verbose = TRUE, parallel_over = "everything", save_workflow = TRUE)
  # metrics     class_prob_metric_set object with performance metrics functions, passed to tune::tune_grid(), e.g. yardstick::metric_set(accuracy, kap, sens, spec, precision, roc_auc)
  # metric.best character  metric used to assess model performance during model training
  # seed.train  numeric    specify a seed for reproducibility of the model training
  # seed.final  numeric    specify a seed for reproducibility of the final model fit
  # plot.tune   logical    if true, plots the performance metric vs. tuned hyperparameters
  # plot.roc    logical    if TRUE, plots the receiver operating curve (ROC)
  
  ## Value
  # Object of class c("tidytune","list"), with these components:
  # 
  ## model fitted on complete training set
  # mod             object of class "workflow" but fitted to complete Xtrain
  # mod$bestTune    data.frame with best hyperparameter combination, including model index (.config)
  # 
  ## model training results
  # train           object of class "tune_results", output from tune_grid()
  # 
  ## info
  # info            list with these components
  # info$call       matched call to tidytune()
  # info$mode       "classification" or "regression" mode
  # info$spec       model type specification, e.g. "discrim_linear"
  # info$engine     engine (package) used for training, e.g. "ranger"
  # info$pkg_fun    <package>::<function> used for training
  # info$nSplit     tibble with number of samples in train, test, total
  # info$nX         number of predictors
  # info$nTune      number of hyperparameter combinations tried, equals nrow(tuneGrid)
  # notes           tibble with notes printed during model training
  # 
  ## predicted classes
  # pred.train      tibble with .pred_class (predicted class), .pred_<classX> (posterior probability for all classes X) for training set
  # pred.test       tibble with .pred_class (predicted class), .pred_<classX> (posterior probability for all classes X) for test set 
  # 
  ## classification model performance metrics  
  # perf.train      list with performance metrics, as outputted by caret::confusionMatrix() for training set
  # perf.test       list with performance metrics, as outputted by caret::confusionMatrix() for test set
  # tidyperf.train  tibble with performance metrics as outputted by collect_metrics(fit_train) %>% dplyr::select(any_of(c(hpar, met_meta)))
  # tidyperf.test   tibble with performance metrics as outputted by fit_final %>% collect_metrics()
  #
  ## reproducibility notes
  # args            list of function arguments used (for reproducibility)
  # time            list with starting time ($t1), finish time ($t2), elapsed time ($elapsed), and information on parallel computation
  
  # # Examples
  # library(tidymodels)
  # library(discrim)
  # 
  # # partition data into training and test set
  # idx <- caret::createDataPartition(y = iris[,"Species"], times = 1, p = 2/3, list = FALSE)[,1]
  # Xtrain <- iris[idx,]
  # Xtest <- iris[-idx,]
  # Ytrain <- iris[idx,"Species"]
  # Ytest <- iris[-idx,"Species"]
  # 
  # # recipe for preprocessing
  # rec <- recipe(formula = Species ~ ., data = tibble(Xtrain)) %>%
  #   step_center(all_numeric_predictors()) %>%
  #   step_pca(all_numeric_predictors(), num_comp = 4)
  # 
  # # specify cross validation
  # folds <- vfold_cv(data = rec$template, v = 10, repeats = 1, strata = "Species", pool = 0.1)
  # 
  # # control tuning process
  # trControl <- control_grid(verbose = TRUE, save_pred = FALSE, save_workflow = TRUE)
  # metrics <- yardstick::metric_set(accuracy, kap, sens, spec)
  # 
  # # specify classification model
  # mod <- parsnip::discrim_linear(mode = "classification", engine = "mda", penalty = tune())
  # show_model_info(class(mod)[1]) ; mod %>% translate() # the "mda" engine accepts <penalty> to be tuned
  # 
  # # specify model training workflow (preprocessing recipe, model fitting)
  # workflow <- workflow(preprocessor = rec, spec = mod)
  # tuneGrid <- expand.grid(penalty = c(0.001,0.01,0.1,1,10,100))
  # 
  # # run tidytune wrapper function
  # res <- tidytune(Xtrain = Xtrain, Ytrain = Ytrain,
  #                 Xtest = Xtest, Ytest = Ytest,
  #                 workflow = workflow, tuneGrid = tuneGrid,
  #                 folds = folds, trControl = trControl,
  #                 metrics = metrics, metric.best = "accuracy")
  
  t1 <- Sys.time()
  
  require(tidymodels)
  
  ## Check input
  stopifnot(inherits(workflow, "workflow"),
            inherits(tuneGrid, c("data.frame","matrix")),
            inherits(folds, "rset"),
            inherits(trControl, "control_resamples"),
            inherits(metrics, "metric_set"),
            metric.best %in% unlist(tibble(metrics = deparse(metrics)) %>%
                                      filter(grepl("UseMethod", metrics)) %>%
                                      mutate(metrics = sub('"\\)', '', 
                                                           sub('[ ]+UseMethod\\("','', 
                                                               metrics)))),
            is.logical(plot.tune), is.logical(plot.roc))
  
  ## Helperfunctions
  # confusionMatrix (copied and simplified from caret::confusionMatrix.table())
  confusionMatrix <- function(data, positive = NULL,
                              prevalence = NULL, mode = "sens_spec", ...){
    if(!(mode %in% c("sens_spec", "prec_recall", "everything")))
      stop("`mode` should be either 'sens_spec', 'prec_recall', or 'everything'")
    if(length(dim(data)) != 2) stop("the table must have two dimensions")
    if(!all.equal(nrow(data), ncol(data))) stop("the table must nrow = ncol")
    if(!isTRUE(all.equal(rownames(data), colnames(data)))) stop("the table must the same classes in the same order")
    if(!is.character(positive) & !is.null(positive)) stop("positive argument must be character")
    
    classLevels <- rownames(data)
    numLevels <- length(classLevels)
    if(numLevels < 2)
      stop("there must be at least 2 factors levels in the data")
    
    if(numLevels == 2 & is.null(positive))  positive <- rownames(data)[1]
    
    
    if(numLevels == 2 & !is.null(prevalence) && length(prevalence) != 1)
      stop("with two levels, one prevalence probability must be specified")
    
    if(numLevels > 2 & !is.null(prevalence) && length(prevalence) != numLevels)
      stop("the number of prevalence probability must be the same as the number of levels")
    
    if(numLevels > 2 & !is.null(prevalence) && is.null(names(prevalence)))
      stop("with >2 classes, the prevalence vector must have names")
    
    propCI <- function(x) {
      res <- try(binom.test(sum(diag(x)), sum(x))$conf.int, silent = TRUE)
      if(inherits(res, "try-error"))
        res <- rep(NA, 2)
      res
    }
    
    propTest <- function(x){
      res <- try(
        binom.test(sum(diag(x)),
                   sum(x),
                   p = max(apply(x, 2, sum)/sum(x)),
                   alternative = "greater"),
        silent = TRUE)
      res <- if(inherits(res, "try-error"))
        c("null.value.probability of success" = NA, p.value = NA)
      else
        res <- unlist(res[c("null.value", "p.value")])
      res
    }
    
    classAgreement <- function(tab, match.names = FALSE) 
    {
      n <- sum(tab)
      ni <- apply(tab, 1, sum)
      nj <- apply(tab, 2, sum)
      if (match.names && !is.null(dimnames(tab))) {
        lev <- intersect(colnames(tab), rownames(tab))
        p0 <- sum(diag(tab[lev, lev]))/n
        pc <- sum(ni[lev] * nj[lev])/n^2
      }
      else {
        m <- min(length(ni), length(nj))
        p0 <- sum(diag(tab[1:m, 1:m]))/n
        pc <- sum((ni[1:m]/n) * (nj[1:m]/n))
      }
      n2 <- choose(n, 2)
      rand <- 1 + (sum(tab^2) - (sum(ni^2) + sum(nj^2))/2)/n2
      nis2 <- sum(choose(ni[ni > 1], 2))
      njs2 <- sum(choose(nj[nj > 1], 2))
      crand <- (sum(choose(tab[tab > 1], 2)) - (nis2 * njs2)/n2)/((nis2 + 
                                                                     njs2)/2 - (nis2 * njs2)/n2)
      list(diag = p0, kappa = (p0 - pc)/(1 - pc), rand = rand, 
           crand = crand)
    }
    
    overall <- c(unlist(classAgreement(data))[c("diag", "kappa")],
                 propCI(data),
                 propTest(data),
                 mcnemar.test(data)$p.value)
    
    names(overall) <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyNull", "AccuracyPValue", "McnemarPValue")
    
    if(numLevels == 2) {
      if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
      negative <- classLevels[!(classLevels %in% positive)]
      tableStats <- c(sensitivity.table(data, positive),
                      specificity.table(data, negative),
                      posPredValue.table(data, positive, prevalence = prevalence),
                      negPredValue.table(data, negative, prevalence = prevalence),
                      precision.table(data, relevant = positive),
                      recall.table(data, relevant = positive),
                      F_meas.table(data, relevant = positive),
                      prevalence,
                      sum(data[positive, positive])/sum(data),
                      sum(data[positive, ])/sum(data))
      names(tableStats) <- c("Sensitivity", "Specificity",
                             "Pos Pred Value", "Neg Pred Value",
                             "Precision", "Recall", "F1",
                             "Prevalence", "Detection Rate",
                             "Detection Prevalence")
      tableStats["Balanced Accuracy"] <- (tableStats["Sensitivity"]+tableStats["Specificity"])/2
      
    } else {
      
      tableStats <- matrix(NA, nrow = length(classLevels), ncol = 11)
      
      for(i in seq(along = classLevels)) {
        pos <- classLevels[i]
        neg <- classLevels[!(classLevels %in% classLevels[i])]
        prev <- if(is.null(prevalence)) sum(data[, pos])/sum(data) else prevalence[pos]
        tableStats[i,] <- c(sensitivity.table(data, pos),
                            specificity.table(data, neg),
                            posPredValue.table(data, pos, prevalence = prev),
                            negPredValue.table(data, neg, prevalence = prev),
                            precision.table(data, relevant = pos),
                            recall.table(data, relevant = pos),
                            F_meas.table(data, relevant = pos),
                            prev,
                            sum(data[pos, pos])/sum(data),
                            sum(data[pos, ])/sum(data), NA)
        tableStats[i,11] <- (tableStats[i,1] + tableStats[i,2])/2
      }
      rownames(tableStats) <- paste("Class:", classLevels)
      colnames(tableStats) <- c("Sensitivity", "Specificity",
                                "Pos Pred Value", "Neg Pred Value",
                                "Precision", "Recall", "F1",
                                "Prevalence", "Detection Rate",
                                "Detection Prevalence", "Balanced Accuracy")
    }
    
    structure(
      list(positive = positive,
           table = data,
           overall = overall,
           byClass = tableStats,
           mode = mode,
           dots = list(...)),
      class = "confusionMatrix")
  }
  
  # sensitivity.table(data, pos),
  sensitivity.table <- function(data, positive = rownames(data)[1], ...) {
    ## "truth" in columns, predictions in rows
    if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
    if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")
    
    if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
      colnames(data) <- rownames(data) <- c("pos", "neg")
      posCol <- which(colnames(tmp) %in% positive)
      negCol <- which(!(colnames(tmp) %in% positive))
      
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      positive <- "pos"
      rm(tmp)
    }
    
    numer <- sum(data[positive, positive])
    denom <- sum(data[, positive])
    sens <- ifelse(denom > 0, numer / denom, NA)
    sens
  }
  
  # specificity.table(data, neg),
  specificity.table <- function(data, negative = rownames(data)[-1], ...) {
    ## "truth" in columns, predictions in rows
    if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
    if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")
    
    if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
      colnames(data) <- rownames(data) <- c("pos", "neg")
      negCol <- which(colnames(tmp) %in% negative)
      posCol <- which(!(colnames(tmp) %in% negative))
      
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      negative <- "neg"
      rm(tmp)
    }
    
    numer <- sum(data[negative, negative])
    denom <- sum(data[, negative])
    spec <- ifelse(denom > 0, numer / denom, NA)
    spec
  }
  
  # posPredValue.table(data, pos, prevalence = prev),
  posPredValue.table <- function(data, positive = rownames(data)[1], prevalence = NULL, ...) {
    ## "truth" in columns, predictions in rows
    if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
    if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")
    
    if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
      colnames(data) <- rownames(data) <- c("pos", "neg")
      posCol <- which(colnames(tmp) %in% positive)
      negCol <- which(!(colnames(tmp) %in% positive))
      
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])      
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      positive <- "pos"
      rm(tmp)
    }
    
    negative <- colnames(data)[colnames(data) != positive]
    if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
    
    sens <- sensitivity(data, positive)
    spec <- specificity(data, negative)
    (sens * prevalence)/((sens*prevalence) + ((1-spec)*(1-prevalence)))
    
  }
  
  # negPredValue.table(data, neg, prevalence = prev),
  negPredValue.table <- function(data, negative = rownames(data)[-1], prevalence = NULL, ...) {
    ## "truth" in columns, predictions in rows
    if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
    if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")
    
    if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
      colnames(data) <- rownames(data) <- c("pos", "neg")
      negCol <- which(colnames(tmp) %in% negative)
      posCol <- which(!(colnames(tmp) %in% negative))
      
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])      
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      negative <- "neg"
      rm(tmp)
    }
    
    positive <- colnames(data)[colnames(data) != negative]
    if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
    
    sens <- sensitivity(data, positive)
    spec <- specificity(data, negative)
    (spec * (1-prevalence))/(((1-sens)*prevalence) + ((spec)*(1-prevalence)))
    
  }
  
  # precision.table(data, relevant = pos),
  precision.table <- function (data, relevant = rownames(data)[1], ...) {
    if (!all.equal(nrow(data), ncol(data)))
      stop("the table must have nrow = ncol")
    if (!all.equal(rownames(data), colnames(data)))
      stop("the table must the same groups in the same order")
    if (nrow(data) > 2) {
      tmp <- data
      data <- matrix(NA, 2, 2)
      colnames(data) <- rownames(data) <- c("rel", "irrel")
      irrelCol <- which(!(colnames(tmp) %in% relevant))
      relCol <- which(colnames(tmp) %in% relevant)
      data[1, 1] <- sum(tmp[relCol, relCol])
      data[1, 2] <- sum(tmp[relCol, irrelCol])
      data[2, 1] <- sum(tmp[irrelCol, relCol])
      data[2, 2] <- sum(tmp[irrelCol, irrelCol])
      data <- as.table(data)
      relevant <- "rel"
      relevant
      rm(tmp)
    }
    numer <- data[relevant, relevant]
    denom <- sum(data[relevant, ])
    spec <- ifelse(denom > 0, numer/denom, NA)
    spec
  }
  
  # recall.table(data, relevant = pos),
  recall.table <- function(data, relevant = rownames(data)[1], ...) {
    if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
    if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")
    
    if(nrow(data) > 2) {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
      colnames(data) <- rownames(data) <- c("rel", "irrel")
      irrelCol <- which(!(colnames(tmp) %in% relevant))
      relCol <- which(colnames(tmp) %in% relevant)
      
      data[1, 1] <- sum(tmp[relCol, relCol])
      data[1, 2] <- sum(tmp[relCol, irrelCol])
      data[2, 1] <- sum(tmp[irrelCol, relCol])
      data[2, 2] <- sum(tmp[irrelCol, irrelCol])
      data <- as.table(data)
      relevant <- "rel"
      rm(tmp)
    }
    numer <- data[relevant, relevant]
    denom <- sum(data[, relevant])
    rec <- ifelse(denom > 0, numer / denom, NA)
    rec
  }
  
  # F_meas.table(data, relevant = pos),
  F_meas.table <- function (data, relevant = rownames(data)[1], beta = 1, ...) {
    prec <- precision.table(data, relevant = relevant)
    rec <- recall.table(data, relevant = relevant)
    (1+beta^2)*prec*rec/((beta^2 * prec)+rec)
  }
  
  ## Set parameters
  rec <- workflow$pre$actions$recipe$recipe
  mod <- workflow$fit$actions$model$spec
  
  y <- filter(rec$var_info, role == "outcome")$variable
  spec <- class(mod)[1]
  mode <- mod$mode
  engine <- mod$engine
  transl <- translate(mod)
  pkg_fun <- paste(transl$method$fit$func, collapse = "_")
  
  met_meta <- c(".metric",".estimator","mean","n","std_err",".config")
  
  ## Model fitting
  totune <- nrow(tuneGrid) > 1 | any(sapply(mod$args, as_label) %in% "tune()")
  if (totune) {
    
    ## Tune hyperparameters using tuneGrid, folds, metrics, trControl
    if (!is.null(seed.train)) {
      .Random.seed <- seed.train
    } else {
      seed.train <- .Random.seed
    }
    fit_train <- try(
      workflow %>% 
        tune_grid(resamples = folds, grid = tuneGrid,
                  metrics = metrics, control = trControl),
      silent = TRUE)
    attr(fit_train, "seed") <- seed.train # use .Random.seed <- attr(fit_train, "seed") to reproduce
    
    if (inherits(fit_train, "try-error")) {
      err <- fit_train[1]
      need.pkg <- grepl("Some package installs are required", err)
      
      # install needed package(s)
      if (need.pkg) {
        need.pkg <- gsub("'\\n|^'", '', sub('[A-Za-z :]+', '', err))
        for (i in need.pkg) {
          message("installing package <", i, ">")
          install.packages(i)
        }
      }
      
      # tune hyperparameters
      if (!is.null(seed.train)) {
        .Random.seed <- seed.train
      } else {
        seed.train <- .Random.seed
      }
      fit_train <- workflow %>% 
        tune_grid(resamples = folds, grid = tuneGrid,
                  metrics = metrics, control = trControl)
      attr(fit_train, "seed") <- seed.train # use .Random.seed <- attr(fit_train, "seed") to reproduce
    }
    
    ## Butcher data in splits component
    for (i in seq(length(fit_train$splits))) {
      fit_train$splits[[i]]$data <- tibble(.rows = nrow(fit_train$splits[[i]]$data))
    }
    
    ## Get best hyperparameter combination (consider select_by*)
    hpar <- colnames(tuneGrid)
    met_train <- collect_metrics(fit_train) %>% # summarizes fit_train$.metric elements
      dplyr::select(any_of(c(hpar, met_meta)))
    
    bestTune <- try(fit_train %>% select_best(metric.best) %>%
                      dplyr::select(any_of(c(hpar, met_meta))),
                    silent = TRUE)
    if (inherits(bestTune, "try-error")) {
      on.exit(return(fit_train))
      stop() 
    }
    
    ## Define final workflow
    wflow_final <- workflow %>% finalize_workflow(bestTune)
    
  } else {
    
    message("Nothing to tune, fitting full training data to workflow")
    
    ## Define final workflow
    fit_train <- bestTune <- met_train <- as_tibble(NA)
    wflow_final <- workflow
    
  }
  
  ## Fit full model with best hyperparameters
  s <- list(data = tibble(rbind(cbind(Xtrain, .Y = Ytrain),
                                cbind(Xtest, .Y = Ytest))) %>%
              try(rename(!!y := .Y), silent = T),
            in_id = 1:nrow(Xtrain),
            out_id = (nrow(Xtrain)+1):(nrow(Xtrain)+nrow(Xtest)),
            id = tibble(id = "Resample1"))
  s <- structure(s, class = c("mc_split","rsplit"))
  
  if (!is.null(seed.final)) {
    .Random.seed <- seed.final
  } else {
    seed.final <- .Random.seed
  }
  fit_final <- last_fit(wflow_final, split = s, metrics = metrics)
  attr(fit_final$.workflow[[1]], "seed") <- seed.final # use .Random.seed <- attr(mod, "seed") to reproduce
  
  ## Predictions
  # training dataset
  if (mode == "classification") {
    pred.train <- tibble(
      predict(fit_final$.workflow[[1]], new_data = training(s), type = "class"),
      predict(fit_final$.workflow[[1]], new_data = training(s), type = "prob"),
      !!y := Ytrain)
  } else {
    pred.train <- predict(fit_final$.workflow[[1]], new_data = training(s))
  }
  
  # test dataset
  if (mode == "classification") {
    pred.test <- tibble(
      predict(fit_final$.workflow[[1]], new_data = testing(s), type = "class"),
      predict(fit_final$.workflow[[1]], new_data = testing(s), type = "prob"),
      !!y := Ytest)
  } else {
    # returns default prediction type
    pred.test <- fit_final$.predictions[[1]] %>% dplyr::select(-c(".row",".config"))
  }
  
  ## Performance
  # met_train see above
  met_final <- fit_final %>% collect_metrics() # also in fit_final$.metrics[[1]]
  
  perf.train <- confusionMatrix(data = table(pred.train$.pred_class, Ytrain, dnn = c("Prediction", "Reference")))
  perf.test <- confusionMatrix(data = table(pred.test$.pred_class, Ytest, dnn = c("Prediction", "Reference")))
  
  
  # plot ROC
  predicted <- levels(droplevels(pred.test[[y]]))
  roc <- try(pred.test %>%
               roc_curve(truth = !!y,
                         estimate = !!(paste(paste0(".pred_", predicted)))),
             silent = TRUE)
  
  ## Butcher data in splits component
  for (i in seq(length(fit_final$splits))) {
    fit_final$splits[[i]]$data <- tibble(.rows = nrow(fit_final$splits[[i]]$data))
  }
  # for (i in seq(length(folds$splits))) {
  #   folds$splits[[i]]$data <- tibble(.rows = nrow(folds$splits[[i]]$data))
  # }
  
  ## List of arguments
  args <- list(Xtrain = tibble(Xtrain), Ytrain = Ytrain, Xtest = tibble(Xtest), Ytest = Ytest,
               # workflow = workflow, # trained workflow stored in res$mod
               # folds = folds, # fold$splits stored in res$train$splits (data removed)
               tuneGrid = tibble(tuneGrid), trControl = trControl,
               metrics = metrics, metric.best = metric.best,
               plot.tune = plot.tune, plot.roc = plot.roc, dots = list(...))
  
  ## Compile results
  nTT <- sapply(list(train = s$in_id, test = s$out_id,
                     total = c(s$in_id, s$out_id)), length)
  t2 <- Sys.time()
  res <- list(mod = fit_final$.workflow[[1]],
              train = fit_train,
              info = list(call = match.call(),
                          mode = mode, spec = spec, engine = engine, pkg_fun = pkg_fun,
                          nSplit = tibble(data.frame(set = names(nTT), n = nTT)),
                          nX = ncol(fit_final$.workflow[[1]]$pre$mold$predictors),
                          nTune = nrow(tuneGrid)),
              notes = fit_final$.notes,
              pred.train = pred.train, pred.test = pred.test,
              perf.train = perf.train, perf.test = perf.test,
              tidyperf.train = met_train, tidyperf.test = met_final,
              args = args,
              time = list(t1 = t1, t2 = t2, elapsed = t2 - t1,
                          parallel = try(c(foreach::getDoParRegistered()), silent = T),
                          name = try(c(foreach::getDoParName()), silent = T),
                          ncpu = try(foreach::getDoParWorkers(), silent = T))
  )
  res$mod$bestTune <- bestTune
  res <- structure(res, class = c("tidytune"))
  
  ## Plot results
  if (totune & plot.tune & !inherits(try(plot(res), silent = TRUE), "try-error")) {print(plot(res))}
  if (totune & plot.roc & !inherits(roc, "try-error")) {try(print(autoplot(roc)), silent = TRUE)}
  
  ## Return
  invisible(res)
}

# plot.tidytune
plot.tidytune <- function(tidytune, order = NULL,
                          x.log10 = TRUE, y.log10 = FALSE, mode = "alpha",
                          point.size = 1, point.alpha = 0.6, line.alpha = 0.6,
                          plot = TRUE) {
  
  ## Arguments
  # order     character vector with hyperparameter names in desired order
  
  require(ggplot2)
  
  ## Get components
  tuneGrid <- tidytune$args$tuneGrid
  bestTune <- tidytune$mod$bestTune
  pkg_fun <- tidytune$info$pkg_fun
  met_train <- tidytune$tidyperf.train #
  
  ## Get order of hyperparameters
  if (is.null(order)) {
    hpar <- colnames(tuneGrid) # change order to change mapping
  } else {
    stopifnot(all(order %in% colnames(tuneGrid)))
    if (!all(colnames(tuneGrid) %in% order)) {
      order <- c(order, colnames(tuneGrid)[!colnames(tuneGrid) %in% order])
    }
    hpar <- order
  }
  
  ## Set plot mode
  cw <- switch(mode, 
               "size" = {
                 dplyr::case_when(ncol(tuneGrid) == 1 ~ {c("x",NA,NA,NA)},
                                  ncol(tuneGrid) == 2 ~ {c("x","color",NA,NA)},
                                  ncol(tuneGrid) == 3 ~ {c("x","color","size",NA)},
                                  ncol(tuneGrid) >= 4 ~ {c("x","color","size","linetype")})
               },
               "alpha" = {
                 dplyr::case_when(ncol(tuneGrid) == 1 ~ {c("x",NA,NA,NA)},
                                  ncol(tuneGrid) == 2 ~ {c("x","color",NA,NA)},
                                  ncol(tuneGrid) == 3 ~ {c("x","color","alpha",NA)},
                                  ncol(tuneGrid) >= 4 ~ {c("x","color","alpha","linetype")})
               })
  
  ## Plot performance metrics
  p.nrow <- ceiling(sqrt(dplyr::n_distinct(met_train$.metric)))
  p <- 
    met_train %>%
    dplyr::mutate(across(!!hpar[-c(which(cw == "x"), which(cw == "size!alpha"))], ~ factor(.x))) %>%
    ggplot(aes_string(x = hpar[1], y = "mean",
                      color = switch(!is.na(cw[2]),hpar[2],NULL),
                      linetype = switch(!is.na(cw[4]),hpar[4],NULL)))
  
  # add points
  if (is.na(cw[3])) {
    p <- p + 
      geom_point(size = point.size, alpha = point.alpha)
  } else {
    switch(mode,
           "size" = {
             p <- p +
               geom_point(aes_string(size = hpar[3]), alpha = point.alpha)
           },
           "alpha" = {
             p <- p +
               geom_point(aes_string(alpha = hpar[3]), size = point.size)
           })
  }
  
  # add lines
  if (is.na(cw[4])) {
    if (is.na(cw[3])) {
      gr <- switch(!is.na(cw[2]),hpar[2],NULL)
    } else {
      gr <- paste0("interaction(", paste0(c(hpar[2],hpar[3]), collapse =  ", "), ")")
    }
    p <- p +
      geom_line(aes_string(group = gr), alpha = line.alpha)
  } else {
    gr <- paste0("interaction(", paste0(c(hpar[2],hpar[3],hpar[4]), collapse =  ", "), ")")
    p <- p +
      geom_line(aes_string(group = gr), alpha = line.alpha)
  }
  if (y.log10) {
    p <- p + scale_y_log10(labels = scales::label_number())
  }
  if (x.log10) {
    if (is.numeric(met_train[[hpar[1]]])) {
      p <- p + scale_x_log10(labels = scales::label_number())
    } else {
      message(hpar[1], " not numeric, no log-transformation done!")
    }
  }
  p <- p + 
    geom_vline(aes(xintercept = as.numeric(bestTune[[hpar[1]]])), linetype = 2) +
    facet_wrap(~ .metric, scales = "fixed", nrow = p.nrow) +
    scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
    theme_bw() +
    ggtitle(paste0("Hyperparameter tuning with ", sub("_", "::", pkg_fun)))
  
  if (plot) print(p)
  invisible(p)
}

# print tidytune object
print.tidytune <- function(tidytune, print.spec = TRUE) {
  
  cat("///////  tidytune  ///\n")
  if (print.spec) {
    cat("\n")
    mprec <- tidytune$mod$pre$actions$recipe$recipe
    mspec <- tidytune$mod$fit$actions$model$spec
    try(print(mprec), silent = T)
    cat("\n\n")
    try(print(mspec), silent = T)
    cat("\n")
  }
  
  ## extract info
  ntrain <- nrow(tidytune$args$Xtrain)
  ntest <- nrow(tidytune$args$Xtest)
  ncla <- nlevels(droplevels(tidytune$args$Ytrain))
  
  nX <- tidytune$info$nX # 
  tuneGrid <- tidytune$args$tuneGrid
  nTune <- nrow(tuneGrid)
  
  perf.train <- tidytune$perf.train
  perf.test <- tidytune$perf.test
  
  ## create strings for printing
  ptrn <- paste0("# training samples: ", stringr::str_pad(ntrain, width = 9))
  ptst <- paste0("# test samples:     ", stringr::str_pad(ntest, width = 9))
  pcla <- paste0("# classes:          ", stringr::str_pad(ncla, width = 9))
  ppar <- paste0("# parameters:       ", stringr::str_pad(nX, width = 9))
  phyp <- paste0("# hyper-par config.:", stringr::str_pad(nTune, width = 9))
  nhyp <- paste0("  tuned hyper-par(s): ", paste(names(tuneGrid), collapse = " ; "))
  
  ## training error across all hyperparameter combinations
  # dacc <- tidytune$tidyperf.train %>%
  #               dplyr::filter(.metric %in% c("accuracy","kap")) %>%
  #               group_by(.metric) %>%
  #               summarise(Mean = mean(mean), SD = sd(mean))
  # acc.train <- cbind(
  #   dacc %>% dplyr::filter(.metric == "accuracy") %>% rename(Accuracy = "Mean", AccuracySD = "SD") %>% select(-1),
  #   dacc %>% dplyr::filter(.metric == "kap") %>% rename(Kappa = "Mean", KappaSD = "SD") %>% select(-1))[c(1,3,2,4)]
  acc.train <-  perf.train$overall["Accuracy"]
  
  ## test set honest prediction accuracy
  # acc.test <-  tidytune$tidyperf.test %>% dplyr::filter(.metric == tidytune$args$metric.best) %>% dplyr::select(".estimate")
  # acc.test <- tidytune$perf.test$overall["Accuracy"] # should be equivalent to the line above
  acc.test <- t(as.matrix(perf.test$overall[1:4]))
  dperclass <- perf.test$byClass[,c("Sensitivity","Specificity","Balanced Accuracy")]
  mperclass <- t(as.matrix(apply(dperclass, 2, mean, na.rm = TRUE)))
  acc.test <- cbind(acc.test,mperclass)
  
  ## print info
  cat("> n                             > p\n")
  cat(paste(ptrn, ppar, sep = "   "), "\n")
  cat(paste(ptst, phyp, sep = "   "), "\n")
  # cat(paste(ptot, nhyp, sep = "   "),"\n")
  cat(paste(pcla, nhyp, sep = "   "),"\n")
  
  cat("\n> performance\n")
  cat(paste0("# training set accuracy: ", stringr::str_pad(round(as.numeric(acc.train["Accuracy"]), 4), width = 7), "\n"))
  cat(paste0("# test set accuracy: ", stringr::str_pad(round(as.numeric(acc.test[1,"Accuracy"]), 4), width = 11), "\n"))
  cat(paste0("# test set bal. accuracy: ", stringr::str_pad(round(as.numeric(acc.test[1,"Balanced Accuracy"]), 4), width = 6), "\n"))
  
}

# summary of tidytune object
summary.tidytune <- function(tidytune) {
  
  ## spec
  spec <- tidytune$info$spec #
  engine <- tidytune$info$engine #
  pkg_fun <- tidytune$info$pkg_fun #
  fun <- sub(".+_(.+)","\\1",pkg_fun) #
  
  ## extract info
  tuneGrid <- tidytune$args$tuneGrid
  perf.train <- tidytune$perf.train
  perf.test <- tidytune$perf.test
  
  ntrain <- nrow(tidytune$args$Xtrain)
  ntest <- nrow(tidytune$args$Xtest)
  ncla <- nlevels(droplevels(tidytune$args$Ytrain))
  
  nX <- tidytune$info$nX #
  tuneGrid <- tidytune$args$tuneGrid
  nTune <- nrow(tuneGrid)
  
  perf.train <- tidytune$perf.train
  perf.test <- tidytune$perf.test
  
  ## training error across all hyperparameter combinations
  #
  
  ## training set error
  perf.train  <- tidytune$perf.train
  acc.train <- t(as.matrix(perf.train$overall[1:4]))
  dperclass <- perf.train$byClass[,c("Sensitivity","Specificity","Balanced Accuracy")]
  mperclass <- t(as.matrix(apply(dperclass, 2, mean, na.rm = TRUE)))
  acc.train <- cbind(acc.train,mperclass)
  colnames(acc.train) <- paste0(colnames(acc.train), "Train")
  
  ## test set honest prediction accuracy
  perf.test  <- tidytune$perf.test
  acc.test <- t(as.matrix(perf.test$overall[1:4]))
  dperclass <- perf.test$byClass[,c("Sensitivity","Specificity","Balanced Accuracy")]
  mperclass <- t(as.matrix(apply(dperclass, 2, mean, na.rm = TRUE)))
  acc.test <- cbind(acc.test,mperclass)
  colnames(acc.test) <- paste0(colnames(acc.test), "Test")
  
  ## stats
  met_meta <- c(".metric",".estimator","mean","n","std_err",".config")
  
  tuned <- apply(tuneGrid, 2, function(x) {paste(range(x), collapse = ":")})
  tuned <- gsub(" ", "", paste0(paste(paste(names(tuned), tuned, sep = "["), collapse = "];"), "]"))
  
  best <- dplyr::as_tibble(tidytune$mod$bestTune) %>% dplyr::select(-any_of(met_meta))
  best <- paste0(paste(paste(names(best), best, sep = "["), collapse = "];"), "]")
  
  ## compile
  dres <- 
    tibble::tibble(.rows = 1) %>%
    dplyr::mutate(spec = spec, engine = engine, fun = fun,
                  tuned = tuned,
                  best = best) %>%
    dplyr::bind_cols(acc.train) %>%
    dplyr::bind_cols(acc.test) %>%
    dplyr::mutate(ntrain = ntrain, ntest = ntest, nX = nX, nclass = ncla,
                  nhypercomb = unlist(nTune),
                  elapsed = c(tidytune$time$delta, tidytune$time$elapsed),
                  ncpu = tidytune$time$ncpu)
  dres <- structure(dres, class = c("summary.tidytune", class(dres)))
  return(dres)
}

# print summary
print.summary.tidytune <- function(summary.tidytune) {
  print(data.frame(summary.tidytune, check.names = FALSE)[,c(1:5,6,13,19,20:23)])
}

predict.tidytune <- function(tidytune, new_data, ...) {
  
  # show_model_info(tidytune$spec) # returns NULL
  predict(tidytune$mod, new_data, ...)
  
}


##################
###### caret #####
##################
# wrapper around caret::train
caret <- function(Xtrain, Ytrain, Xtest, Ytest, 
                  method = "rf", preProcess = NULL,
                  metric = "Accuracy", tuneGrid = expand.grid(.mtry = floor(sqrt(ncol(Xtrain)))),
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid"),
                  print = TRUE, plot = TRUE, strip = TRUE, ...) {
  
  t1 <- Sys.time()
  require(caret)
  
  # check input
  stopifnot(inherits(Xtrain, c("data.frame","matrix")),
            inherits(Xtest, c("data.frame","matrix")),
            is.factor(Ytrain), is.factor(Ytest),
            identical(levels(Ytrain), levels(Ytest)),
            inherits(tuneGrid, "data.frame"),
            inherits(trControl, "list"))
  
  # train
  trn <- try(train(x = Xtrain, y = Ytrain, method = method, preProcess = preProcess,
                   metric = metric, tuneGrid = tuneGrid, trControl = trControl, ...), silent = TRUE)
  
  # confusion matrices
  if (inherits(trn, "try-error")) {
    cat(trn[1])
    print <- FALSE
    plot <- FALSE
    trn <- list(error = paste0("caret::train() gave the following ", trn[1]), results = array(NA, dim = c(1,4)))
    pred.train <- pred.test <- perf.train <- perf.test <- list(table = NA, positive = NA, overall = NA, byClass = NA)
  } else {
    pred.train <- predict(trn, Xtrain)
    # pred.train <- trn$trainingData$.outcome
    pred.test <- predict(trn, Xtest)
    perf.train <- confusionMatrix(pred.train, Ytrain)
    perf.test  <- confusionMatrix(pred.test, Ytest)
  }
  
  # print results
  if (print) {
    print(trn)
    if (nrow(trn$results) > 0) {cat("\nPerformance on resampled training set:\n") ; print(trn$results)}
    cat("\nPerformance on full training set:\n")  
    print(perf.train$overall)
    cat("\nPerformance on test set:\n")
    print(perf.test$overall)
  }
  
  # plot results
  if (plot & trControl$method != "none" & nrow(trn$results) > 1) {
    print(plot(trn))
  }
  
  # stip model
  if (strip) {
    # trainingData is already stored in $args$Xtrain
    trn$trainingData <- NA
  }  
  # list of arguments
  args <- list(Xtrain = Xtrain, Ytrain = Ytrain, Xtest = Xtest, Ytest = Ytest,
               method = method, preProcess = preProcess, metric = metric,
               tuneGrid = tuneGrid, trControl = trControl, print = print,
               plot = plot, strip = strip, dots = list(...))
  
  # return results
  t2 <- Sys.time()
  res <- list(mod = trn, pred.train = pred.train, pred.test = pred.test, perf.train = perf.train, perf.test = perf.test,
              args = args, time = list(t1 = t1, t2 = t2, elapsed = t2 - t1))
  res <- structure(res, class = c("caret"))
  invisible(res)
}

# plot caret object
plot.caret <- function(caret, order = NULL,
                       x.log10 = TRUE, y.log10 = FALSE, mode = "alpha",
                       point.size = 1, point.alpha = 0.6, line.alpha = 0.6,
                       plot = TRUE) {
  
  require(ggplot2)
  require(dplyr)
  
  ## Get components
  tuneGrid <- caret$args$tuneGrid
  bestTune <- caret$mod$bestTune
  pkg_fun <- if (!is.null(caret$info$pkg_fun)) {
    caret$info$pkg_fun
  } else if (is.null(caret$mod$modelInfo$fit) & all(c("fm","lev","ni") %in% names(caret$mod))) {
    switch(class(caret$mod)[1], "Kplsrda" = "rchemo_kplsrda")
  } else {
    l <- deparse(caret$mod$modelInfo$fit)
    l <- unlist(strsplit(l[grep("::", l)[1]], split = " "))
    gsub("([A-Za-z._]+)[:]+([A-Za-z._]+)", "\\1_\\2",
         sub("([A-Za-z.:_]+)\\(.*$", "\\1",
             l[grep("::", l)[1]]))
  } #
  
  ## Reshape training $mod$results table (comparable with tidytune()$tidyperf.train) 
  # > met_train # desired format
  # <param1> <param2> .metric   mean     sd
  #       1        5   Accuracy 0.524 0.0394
  #       2        6   Kappa    0.479 0.0448
  met_train <- switch(class(caret$mod)[1],
                      "Kplsrda" = {
                        tibble(caret$train) %>%
                          mutate(.metric = "Accuracy") %>%
                          mutate(mean = 1-mean) %>% # Accuracy = 1-Error
                          select(-any_of(c("y","n","min","max","se","median","lwr.ci","upr.ci")))
                      },
                      "kplsrda" = {
                        if (identical(caret$args$score, rchemo::err)) {
                          # old train.kplsrda() with err
                          tibble(caret$train) %>%
                            mutate(.metric = "Accuracy") %>%
                            mutate(mean = 1-mean) %>% # Accuracy = 1-Error
                            select(-any_of(c("y","n","min","max","se","median","lwr.ci","upr.ci")))
                        } else if (identical(caret$args$score, rchemo::msep) & "gamma" %in% names(caret$mod$bestTune)) {
                          # old train.kplsrda() with msep
                          tibble(caret$train) %>%
                            group_by(gamma, nlv) %>%
                            summarize(kern = unique(kern), gamma = unique(gamma),
                                      nlv = unique(nlv), mean = mean(mean),
                                      .groups = "keep") %>%
                            mutate(.metric = "msep") %>%
                            select(-any_of(c("y","n","min","max","se","median","lwr.ci","upr.ci"))) %>%
                            ungroup()
                        } else if (identical(caret$args$score, rchemo::msep) & "sigma" %in% names(caret$mod$bestTune)) {
                          # old train.kplsrda() with msep
                          tibble(caret$train) %>%
                            group_by(sigma, nlv) %>%
                            summarize(kern = unique(kern), sigma = unique(sigma),
                                      nlv = unique(nlv), mean = mean(mean),
                                      .groups = "keep") %>%
                            mutate(.metric = "msep") %>%
                            select(-any_of(c("y","n","min","max","se","median","lwr.ci","upr.ci"))) %>%
                            ungroup()
                        } else {
                          # new train.kplsrda() with err that gets translated to accuracy
                          tibble(caret$train) %>%
                            mutate(.metric = "Accuracy") %>%
                            select(-any_of(c("y","n","min","max","se","median","lwr.ci","upr.ci")))
                        }
                      },
                      "train" = {
                        if (!is.list(caret$mod.tune)) {
                          results <- caret$mod$results # caret
                        } else {
                          results <- caret$mod.tune$results # caret dakpc
                          if (all(dim(bestTune) == 1)) {
                            bestTune <- data.frame(nlv = ncol(caret$pca.kern$T),
                                                   gamma = caret$pca.kern$dots$gamma)
                          }
                        }
                        tuned <- names(tuneGrid)
                        perf <- names(results)[!names(results) %in% tuned]
                        perf.sd <- perf[grep("SD$|Lower$|Upper$|Null$|PValue$", perf)]
                        
                        if (any(grepl("SD$", perf.sd))) {
                          merge(
                            results %>%
                              dplyr::mutate(.id = seq(nrow(results))) %>%
                              tidyr::pivot_longer(cols = any_of(perf), values_to = "mean") %>%
                              dplyr::filter(!name %in% perf.sd) %>%
                              mutate(.merge = name),
                            results %>%
                              dplyr::mutate(.id = seq(nrow(results))) %>%
                              tidyr::pivot_longer(cols = any_of(perf), values_to = "sd") %>%
                              dplyr::filter(name %in% perf.sd) %>%
                              mutate(.merge = sub("SD$", "", name)) %>%
                              select(-any_of(c(tuned,"name"))),
                            by = c(".id", ".merge")
                          ) %>% tibble() %>%
                            select(-any_of(c(".id",".merge"))) %>%
                            rename(.metric = name)
                        } else if (any(grepl("Lower$|Upper$|Null$|PValue$", perf.sd))) {
                          results %>%
                            dplyr::mutate(.id = seq(nrow(results))) %>%
                            tidyr::pivot_longer(cols = any_of(perf), values_to = "mean") %>%
                            dplyr::filter(!name %in% perf.sd) %>%
                            mutate(.merge = name) %>%
                            select(-any_of(c(".id",".merge"))) %>%
                            rename(.metric = name)
                        }
                      }
  )
  
  ## Get order of hyperparameters
  if (is.null(order)) {
    hpar <- colnames(tuneGrid) # change order to change mapping
  } else {
    stopifnot(all(order %in% colnames(tuneGrid)))
    if (!all(colnames(tuneGrid) %in% order)) {
      order <- c(order, colnames(tuneGrid)[!colnames(tuneGrid) %in% order])
    }
    hpar <- order
  }
  
  ## Set plot mode
  switch(mode, 
         "size" = {
           cw <- dplyr::case_when(ncol(tuneGrid) == 1 ~ {c("x",NA,NA,NA)},
                                  ncol(tuneGrid) == 2 ~ {c("x","color",NA,NA)},
                                  ncol(tuneGrid) == 3 ~ {c("x","color","size",NA)},
                                  ncol(tuneGrid) >= 4 ~ {c("x","color","size","linetype")})
         },
         "alpha" = {
           cw <- dplyr::case_when(ncol(tuneGrid) == 1 ~ {c("x",NA,NA,NA)},
                                  ncol(tuneGrid) == 2 ~ {c("x","color",NA,NA)},
                                  ncol(tuneGrid) == 3 ~ {c("x","color","alpha",NA)},
                                  ncol(tuneGrid) >= 4 ~ {c("x","color","alpha","linetype")})
         })
  
  ## Plot performance metrics
  p.nrow <- ceiling(sqrt(dplyr::n_distinct(met_train$.metric)))
  p <- 
    met_train %>%
    dplyr::mutate(across(!!hpar[-c(which(cw == "x"), which(cw == "size!alpha"))], ~ factor(.x))) %>%
    ggplot(aes_string(x = hpar[1], y = "mean",
                      color = switch(!is.na(cw[2]),hpar[2],NULL),
                      linetype = switch(!is.na(cw[4]),hpar[4],NULL)))
  
  # add points
  if (is.na(cw[3])) {
    p <- p + 
      geom_point(size = point.size, alpha = point.alpha)
  } else {
    switch(mode,
           "size" = {
             p <- p +
               geom_point(aes_string(size = hpar[3]), alpha = point.alpha)
           },
           "alpha" = {
             p <- p +
               geom_point(aes_string(alpha = hpar[3]), size = point.size)
           })
  }
  
  # add lines
  if (is.na(cw[4])) {
    if (is.na(cw[3])) {
      gr <- switch(!is.na(cw[2]),hpar[2],NULL)
    } else {
      gr <- paste0("interaction(", paste0(c(hpar[2],hpar[3]), collapse =  ", "), ")")
    }
    p <- p +
      geom_line(aes_string(group = gr), alpha = line.alpha)
  } else {
    gr <- paste0("interaction(", paste0(c(hpar[2],hpar[3],hpar[4]), collapse =  ", "), ")")
    p <- p +
      geom_line(aes_string(group = gr), alpha = line.alpha)
  }
  if (y.log10) {
    p <- p + scale_y_log10(labels = scales::label_number())
  }
  if (x.log10) {
    if (is.numeric(met_train[[hpar[1]]])) {
      p <- p + scale_x_log10(labels = scales::label_number())
    } else {
      message(hpar[1], " not numeric, no log-transformation done!")
    }
  }
  p <- p +
    geom_vline(aes(xintercept = bestTune[[hpar[1]]]), linetype = 2) +
    facet_wrap(~ .metric, scales = "fixed", nrow = p.nrow) +
    scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
    theme_bw() +
    ggtitle(paste0("Hyperparameter tuning with ", sub("_", "::", pkg_fun)))
  
  if (plot) print(p)
  invisible(p)
}

# print caret
print.caret <- function(caret, print.spec = TRUE) {
  
  switch(class(caret$mod)[1],
         "train" = {
           cat("///////  caret  ///\n")
         },
         "kplsrda" = {
           cat("///////  rchemo  ///\n")
         })
  if (print.spec) {
    cat("\n")
    try(print(caret$mod),silent=T)
    cat("\n")
  }
  
  ## extract info
  ntrain <- nrow(caret$args$Xtrain)
  ntest <- nrow(caret$args$Xtest)
  ncla <- nlevels(droplevels(caret$args$Ytrain))
  
  nX <- ncol(caret$args$Xtrain) #
  tuneGrid <- caret$args$tuneGrid
  nTune <- nrow(tuneGrid)
  
  perf.train <- caret$perf.train
  perf.test <- caret$perf.test
  
  ## create strings for printing
  ptrn <- paste0("# training samples: ", stringr::str_pad(ntrain, width = 9))
  ptst <- paste0("# test samples:     ", stringr::str_pad(ntest, width = 9))
  pcla <- paste0("# classes:          ", stringr::str_pad(ncla, width = 9))
  ppar <- paste0("# parameters:       ", stringr::str_pad(nX, width = 9))
  phyp <- paste0("# hyper-par config.:", stringr::str_pad(nTune, width = 9))
  nhyp <- paste0("  tuned hyper-par(s): ", paste(names(tuneGrid), collapse = " ; "))
  
  ## training error across all hyperparameter combinations
  # dacc <- tidytune$tidyperf.train %>%
  #               dplyr::filter(.metric %in% c("accuracy","kap")) %>%
  #               group_by(.metric) %>%
  #               summarise(Mean = mean(mean), SD = sd(mean))
  # acc.train <- cbind(
  #   dacc %>% dplyr::filter(.metric == "accuracy") %>% rename(Accuracy = "Mean", AccuracySD = "SD") %>% select(-1),
  #   dacc %>% dplyr::filter(.metric == "kap") %>% rename(Kappa = "Mean", KappaSD = "SD") %>% select(-1))[c(1,3,2,4)]
  acc.train <-  perf.train$overall["Accuracy"]
  
  ## test set honest prediction accuracy
  # acc.test <-  tidytune$tidyperf.test %>% dplyr::filter(.metric == tidytune$args$metric.best) %>% dplyr::select(".estimate")
  # acc.test <- tidytune$perf.test$overall["Accuracy"] # should be equivalent to the line above
  acc.test <- t(as.matrix(perf.test$overall[1:4]))
  dperclass <- perf.test$byClass[,c("Sensitivity","Specificity","Balanced Accuracy")]
  mperclass <- t(as.matrix(apply(dperclass, 2, mean, na.rm = TRUE)))
  acc.test <- cbind(acc.test,mperclass)
  
  ## print info
  cat("> n                             > p\n")
  cat(paste(ptrn, ppar, sep = "   "), "\n")
  cat(paste(ptst, phyp, sep = "   "), "\n")
  # cat(paste(ptot, nhyp, sep = "   "),"\n")
  cat(paste(pcla, nhyp, sep = "   "),"\n")
  
  cat("\n> performance\n")
  cat(paste0("# training set accuracy: ", stringr::str_pad(round(as.numeric(acc.train["Accuracy"]), 4), width = 7), "\n"))
  cat(paste0("# test set accuracy: ", stringr::str_pad(round(as.numeric(acc.test[1,"Accuracy"]), 4), width = 11), "\n"))
  cat(paste0("# test set bal. accuracy: ", stringr::str_pad(round(as.numeric(acc.test[1,"Balanced Accuracy"]), 4), width = 6), "\n"))
  
}

# summary of caret object
summary.caret <- function(caret) {
  
  ## spec
  spec <- caret$mod$modelInfo$label #
  engine <- caret$mod$modelInfo$library #
  pkg_fun <- if (!is.null(caret$info$pkg_fun)) {
    caret$info$pkg_fun
  } else if (is.null(caret$mod$modelInfo$fit) & all(c("fm","lev","ni") %in% names(caret$mod))) {
    switch(class(caret$mod)[1], "Kplsrda" = "rchemo_kplsrda")
  } else {
    l <- deparse(caret$mod$modelInfo$fit)
    l <- unlist(strsplit(l[grep("::", l)[1]], split = " "))
    gsub("([A-Za-z._]+)[:]+([A-Za-z._]+)", "\\1_\\2",
         sub("([A-Za-z.:_]+)\\(.*$", "\\1",
             l[grep("::", l)[1]]))
  } #
  fun <- sub(".+_(.+)","\\1",pkg_fun) #
  
  ## extract info
  tuneGrid <- caret$args$tuneGrid
  perf.train <- caret$perf.train
  perf.test <- caret$perf.test
  
  ntrain <- nrow(caret$args$Xtrain)
  ntest <- nrow(caret$args$Xtest)
  ncla <- nlevels(droplevels(caret$args$Ytrain))
  
  nX <- ncol(caret$args$Xtrain) #
  tuneGrid <- caret$args$tuneGrid
  nTune <- nrow(tuneGrid)
  
  perf.train <- caret$perf.train
  perf.test <- caret$perf.test
  
  ## training error across all hyperparameter combinations
  #
  
  ## training set error
  perf.train  <- caret$perf.train
  acc.train <- t(as.matrix(perf.train$overall[1:4]))
  dperclass <- perf.train$byClass[,c("Sensitivity","Specificity","Balanced Accuracy")]
  mperclass <- t(as.matrix(apply(dperclass, 2, mean, na.rm = TRUE)))
  acc.train <- cbind(acc.train,mperclass)
  colnames(acc.train) <- paste0(colnames(acc.train), "Train")
  
  ## test set honest prediction accuracy
  perf.test  <- caret$perf.test
  acc.test <- t(as.matrix(perf.test$overall[1:4]))
  dperclass <- perf.test$byClass[,c("Sensitivity","Specificity","Balanced Accuracy")]
  mperclass <- t(as.matrix(apply(dperclass, 2, mean, na.rm = TRUE)))
  acc.test <- cbind(acc.test,mperclass)
  colnames(acc.test) <- paste0(colnames(acc.test), "Test")
  
  ## stats
  met_meta <- c(".metric",".estimator","mean","n","std_err",".config")
  
  tuned <- apply(tuneGrid, 2, function(x) {paste(range(x), collapse = ":")})
  tuned <- gsub(" ", "", paste0(paste(paste(names(tuned), tuned, sep = "["), collapse = "];"), "]"))
  
  best <- dplyr::as_tibble(caret$mod$bestTune) %>% dplyr::select(-any_of(met_meta))
  best <- paste0(paste(paste(names(best), best, sep = "["), collapse = "];"), "]")
  
  ## compile
  dres <- 
    tibble::tibble(.rows = 1) %>%
    dplyr::mutate(spec = spec, engine = engine, fun = fun,
                  tuned = tuned,
                  best = best) %>%
    dplyr::bind_cols(acc.train) %>%
    dplyr::bind_cols(acc.test) %>%
    dplyr::mutate(ntrain = ntrain, ntest = ntest, nX = nX, nclass = ncla,
                  nhypercomb = unlist(nTune),
                  elapsed = c(caret$time$delta, caret$time$elapsed),
                  ncpu = caret$time$ncpu)
  dres <- structure(dres, class = c("summary.caret", class(dres)))
  return(dres)
}

# print caret summary
print.summary.caret <- function(summary.caret) {
  print(data.frame(summary.caret, check.names = FALSE)[,c(1:5,6,13,19,20:23)])
}

# predict from tidytune
predict.caret <- function(caret, new_data, ...) {
  
  # show_model_info(tidytune$spec) # returns NULL
  predict(caret$mod, new_data, ...)
  
}

# wrapper around rchemo::gridcvlv
train.kplsrda <- function(Xtrain, Ytrain, Xtest, Ytest, method = "kplsrda", kern = "krbf",
                          tuneGrid = expand.grid(nlv = unique(round(seq(1, nrow(Xtrain), length.out = 100))), gamma = c(0.01,0.1,1,10)),
                          cv = TRUE, segm = segmkf(n = nrow(Xtrain), type = "random", K = 10, nrep = 3),
                          score = err, maximize = FALSE, verbose = TRUE, plot = TRUE) {
  require(rchemo) # gridcvlv
  require(caret)  # confusionMatrix
  require(ggplot2)
  t1 <- Sys.time()
  
  # check input
  fun <- eval(parse(text = method))
  stopifnot(inherits(Xtrain, c("matrix","data.frame")),
            inherits(Xtest, c("matrix","data.frame")),
            is.factor(Ytrain), length(Ytrain) == nrow(Xtrain),
            is.factor(Ytest), length(Ytest) == nrow(Xtest),is.list(segm),
            is.function(score), is.logical(maximize), is.function(fun),
            kern %in% c("krbf","kpol","ktanh"), is.logical(cv), 
            is.data.frame(tuneGrid), "nlv" %in% names(tuneGrid),
            is.logical(verbose), is.logical(plot))
  
  # get parameter ranges
  ranges <- apply(tuneGrid, 2, function(x) sort(unique(x)), simplify=F)
  hranges <- ranges[!names(ranges) == "nlv",drop=F]
  hGrid <- tuneGrid[,!colnames(tuneGrid) == "nlv",drop=F]
  hGrid <- hGrid[!duplicated(hGrid),,drop=F]
  pars <- as.list(data.frame(kern = kern, hGrid)) # pass kern and gamma grid here
  # pars <- c(pars, list(maxit = rep(1E06, lengths(pars)[1])))
  
  # cv
  if (cv) {
    # tuning nlv and hyperparameters
    dd <- rchemo::gridcvlv(X = as.matrix(Xtrain), Y = as.numeric(Ytrain),
                           segm = segm, score = err,
                           verb = verbose, fun = fun, nlv = ranges[["nlv"]],
                           pars = pars)
    dd.train <- data.frame(summary.by(.data = dd$val_rep, group_by = c("nlv", names(pars)), y = "y1"))
    
    # best nlv and hyperparameters
    selfun <- if (maximize) which.max else which.min
    dd.best <- dd.train[do.call(what = selfun, args = list(x = dd.train$mean)),]
    par.best <- sapply(c("nlv", names(pars)[!names(pars) == "kern"]), FUN = function(x) {dd.best[,x]}, simplify = FALSE)
    nlv.best <- par.best$nlv
    par.col <- names(par.best[-1])[1]
    
    # plot scores
    p <- ggplot(dd.train) +
      geom_line(aes_string(x = "nlv", y = "mean", group = par.col, colour = par.col)) +
      geom_vline(aes(xintercept = nlv.best)) +
      labs(x = "Number of Components (nlv)", y = paste0(deparse(substitute(score)), "() score")) +
      theme_bw() +
      ggtitle(paste0("kern = ", paste0(unique(dd.train$kern), collapse = ",")))
    
    if (!all(is.na(dd.train$sd))) {
      p <- p +
        geom_ribbon(inherit.aes = T, aes_string(x = "nlv", y = "mean", ymin = "lwr.ci", ymax = "upr.ci", group = par.col), color = NA, alpha = 0.1)
    }
    if (plot) {
      print(p)
    }
  } else {
    res <- list(train = NA, plot = NA)
    stopifnot(all(lengths(ranges) == 1))
    dd.train <- p <- NA
    par.best <- apply(tuneGrid, 2, unique, simplify = FALSE)
    nlv.best <- par.best$nlv
  }
  
  # fit model on all training data
  iname <- paste0(paste(paste(names(par.best), as.numeric(unlist(par.best)), sep = "["), collapse = "];"), "]")
  if (verbose) cat("Fitting", iname, "on full training set\n")
  mod <- do.call(what = fun, args = c(list(X = as.matrix(Xtrain), y = as.numeric(Ytrain),
                                           nlv = nlv.best), c(list(kern = kern), par.best[-1])))
  
  # add bestTune some class "tune" components (EDIT: predict.tune will not work)
  mod[["method"]] <- method
  mod[["modelInfo"]] <- list(label = "KPLSR-DA", library = "rchemo",
                             loop = NULL, type = "Classification",
                             parameters = data.frame(parameter = c("nlv","gamma"), class = c("numeric"),
                                                     label = c("#Latent Variables","Gamma")))
  mod[["modelType"]] <- "Classification"
  mod[["results"]] <- dd.train
  mod[["bestTune"]] <- data.frame(t(unlist(par.best)),check.names=F)
  mod[["metric"]] <- "Accuracy"
  mod[["levels"]] <- levels(Ytrain)
  mod <- structure(mod, class = c("kplsrda", "Kplsrda"))
  
  # predict test samples
  
  # without predict.kplsrda
  # pred.train <- factor(levels(Ytrain)[predict(mod, as.matrix(Xtrain))$pred[,1]], levels = levels(Ytrain))
  # pred.test <- factor(levels(Ytrain)[predict(mod, as.matrix(Xtest))$pred[,1]], levels = levels(Ytrain))
  
  # with predict.kplsrda
  pred.train <- predict(mod, Xtrain)
  pred.test <- predict(mod, Xtest)
  
  # performance
  perf.train <- confusionMatrix(pred.train, Ytrain)
  perf.test  <- confusionMatrix(pred.test, Ytest)
  
  # list of arguments
  args <- list(Xtrain = Xtrain, Ytrain = Ytrain, Xtest = Xtest, Ytest = Ytest,
               method = method, fun = fun, kern = kern,
               tuneGrid = tuneGrid, cv = cv, segm = segm,
               score = score, maximize = maximize, verbose = verbose, plot = plot)
  
  # list of model info
  info <- list(call = match.call(), mode = "classification",
               spec = "Kplsrda", engine = "rchemo", pkg_fun = paste0("rchemo_", method),
               nSplit = data.frame(set = c("train","test","total"),
                                   n   = c(nrow(Xtrain), nrow(Xtest),
                                           nrow(Xtrain) + nrow(Xtest))),
               nX = ncol(Xtrain), nTune = nrow(tuneGrid))
  
  # return results
  t2 <- Sys.time()
  res <- list(mod = mod, train = dd.train, info = info, notes = NA, plot = p,
              pred.train = pred.train, pred.test = pred.test, 
              perf.train = perf.train, perf.test = perf.test,
              args = args, time = list(t1 = t1, t2 = t2, elapsed = t2 - t1))
  res <- structure(res, class = "caret")
  invisible(res)
}

# predict caret object where $mod is not class "train" but class "kplsrda" (own) and "Kplsrda" (rchemo)
predict.kplsrda <- function(object, newdata = NULL, type = "class", transf = "raw", ...) {
  
  ## Arguments
  # type      class or prob
  # transf    if "raw", returns raw posterior probabilities (output of predict.Kplsrda)
  #           if "softmax", transforms raw PP using the softmax function
  #           if "unit", transforms raw PP by scaling them into [0,1]
  
  # check input
  stopifnot(type %in% c("class","prob"),
            transf %in% c("raw","softmax","unit"))
  
  levels <- if(!is.null(object$levels)) {
    object$levels
  } else if (!is.null(object$y)) {
    levels(object$y)
  } else if (!is.null(object$fm$C)) {
    rownames(object$fm$C)
  }
  
  switch(type,
         "class" = {
           out <- predict.Kplsrda(object, newdata, ...)$pred
           factor(levels[out[,1]], levels = levels)
         },
         "prob" = {
           prob <- predict.Kplsrda(object, newdata, ...)$posterior
           colnames(prob) <- levels
           
           ## rescale prob to [0,1]
           # https://rpubs.com/FJRubio/softmax
           softmax <- function(par){
             n.par <- length(par)
             par1 <- sort(par, decreasing = TRUE)
             Lk <- par1[1]
             for (k in 1:(n.par-1)) {
               Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
             }
             val <- exp(par - Lk)
             return(val)
           }
           switch(transf,
                  "raw" = {
                    data.frame(prob, check.names = FALSE)
                  },
                  "softmax" = {
                    data.frame(t(apply(prob, 1, softmax)), check.names = FALSE)
                  },
                  "unit" = {
                    data.frame(t(apply(prob, 1, scales::rescale, to = c(0,1))), check.names = FALSE)
                  })
           
         }
  )
}

# print kplsrda object
print.kplsrda <- function(object) {
  
  cat(paste0("Kernel Partial-Least-Squares Disciminant Analysis (", object$modelInfo$label, ")\n\n"))
  
  cat(paste0(length(object$levels), " classes:\n"))
  print(object$levels)
  
  cat("\nBest tuning hyperparameters:\n")
  print(object$bestTune)
  
}

# train nlv and gamma in kernel (kplsrda) simultaneously
train.dakpc <- function(Xtrain, Ytrain, Xtest, Ytest, kern = "krbf",
                        tuneGrid = expand.grid(nlv = unique(c(1, seq(5, min(ncol(Xtrain), 300), by = 5))),
                                               gamma = c(0.001, 0.01, seq(0.05, 0.5, by = 0.05), 1, seq(5, 50, by = 5))),
                        metric = "Accuracy", verbose = TRUE, plot = TRUE,
                        control = trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid")) {
  require(rchemo)
  require(caret)
  t1 <- Sys.time()
  
  # check input
  stopifnot(inherits(Xtrain, c("matrix","data.frame")),
            inherits(Xtest, c("matrix","data.frame")),
            is.factor(Ytrain), length(Ytrain) == nrow(Xtrain),
            is.factor(Ytest), length(Ytest) == nrow(Xtest),
            kern %in% c("krbf","kpol","ktanh"),
            is.data.frame(tuneGrid), all(c("nlv","gamma") %in% colnames(tuneGrid)),
            is.numeric(tuneGrid$nlv), all(tuneGrid$nlv > 0), is.numeric(tuneGrid$gamma),
            metric %in% c("Accuracy","Kappa","RMSE","Rsquared"),
            is.list(control), all(c("method","number","repeats","search") %in% names(control)),
            is.logical(verbose), is.logical(plot), is.function(caret))
  
  # get pc.range and gamma.range
  pc.range <- sort(unique(tuneGrid$nlv))
  gamma.range <- sort(unique(tuneGrid$gamma))
  
  # grid search
  dd.gamma.train <- data.frame(array(NA, dim = c(0, 4)))
  dd.gamma.test <- dd.gamma.train
  for (gamma in gamma.range) {
    pca.kern <- rchemo::kpca(Xtrain, weights = NULL, nlv = ncol(Xtrain), kern = kern, gamma = gamma)
    pca.kern$test <- rchemo::transform(pca.kern, Xtest)
    
    for (npca in pc.range) {
      if (verbose) cat(paste0("gamma: ", gamma, " ; pc 1:", npca, "\n"))
      
      # LDA in KPCA space
      # set.seed(13981)
      res <- try(caret(Xtrain = pca.kern$T[,1:npca,drop=F], Ytrain = Ytrain,
                       Xtest = pca.kern$test[,1:npca,drop=F], Ytest = Ytest,
                       method = "lda", metric = metric,
                       trControl = control,
                       print = FALSE, tuneGrid = expand.grid(parameter = "parameter")),
                 silent = TRUE)
      
      # bind
      if (!inherits(res, "try-error") & !all(is.na(res$perf.train))) {
        dd.gamma.train <- rbind(dd.gamma.train,
                                cbind(data.frame("nlv" = npca, "gamma" = gamma,
                                                 t(res$perf.train$overall))))
        dd.gamma.test <- rbind(dd.gamma.test,
                               cbind(data.frame("nlv" = npca, "gamma" = gamma,
                                                t(res$perf.test$overall))))
      }
    }
  }
  
  # determine best according to training data
  rbf.gamma <- dd.gamma.train[which.max(dd.gamma.train[,metric]),"gamma"]
  rbf.npca <-  dd.gamma.train[which.max(dd.gamma.train[,metric]),"nlv"]
  par.best <- data.frame(nlv = rbf.npca, gamma = rbf.gamma)
  
  # plot results
  dd.gamma2 <- rbind(data.frame(dd.gamma.train, set = "train"),
                     data.frame(dd.gamma.test, set = "test"))
  dd.gamma2$set <- factor(dd.gamma2$set, levels = c("train", "test"))
  rownames(dd.gamma2) <- NULL
  
  plot.gamma <- ggplot(dd.gamma2, aes_string(x = "nlv", y = metric, group = "gamma", color = "gamma")) +
    geom_line() +
    geom_vline(aes(xintercept = rbf.npca)) +
    labs(x = "Number of retained PC", y = metric, color = paste0("gamma\n(", sub("^k", "", kern),  " kernel)")) +
    facet_wrap(~set) +
    theme_bw()
  if (plot) print(plot.gamma)
  
  # fit final model 
  iname <- paste0(paste(paste(names(par.best), as.numeric(unlist(par.best)), sep = "["), collapse = "];"), "]")
  if (verbose) cat("Fitting", iname, "on full training set\n")
  
  pca.kern <- rchemo::kpca(Xtrain, weights = NULL, nlv = par.best$nlv, kern = kern, gamma = par.best$gamma)
  pca.kern$test <- rchemo::transform(pca.kern, Xtest)
  
  mod <- try(caret(Xtrain = pca.kern$T, Ytrain = Ytrain, 
                   Xtest = pca.kern$test, Ytest = Ytest, 
                   method = "lda", metric = metric, trControl = control,
                   print = FALSE, tuneGrid = tuneGrid),
             silent = TRUE)
  mod[["results"]] <- dd.gamma.train
  mod[["results.test"]] <- dd.gamma.test
  mod[["bestTune"]] <- par.best
  
  # list of arguments
  args <- list(Xtrain = Xtrain, Ytrain = Ytrain, Xtest = Xtest, Ytest = Ytest,
               method = "dakpc", kern = kern, tuneGrid = tuneGrid,
               metric = metric, verbose = verbose, plot = plot, control = control)
  
  # list of model info
  info <- list(call = match.call(), mode = "classification",
               spec = "dakpc", engine = "rchemo", pkg_fun = paste0("rchemo_", "kpca"),
               nSplit = data.frame(set = c("train","test","total"),
                                   n   = c(nrow(Xtrain), nrow(Xtest),
                                           nrow(Xtrain) + nrow(Xtest))),
               nX = ncol(Xtrain), nTune = nrow(tuneGrid))
  
  # return results
  t2 <- Sys.time()
  res <- list(mod = mod$mod, mod.tune = mod, info = info, notes = NA,
              pred.train = mod$pred.train, pred.test = mod$pred.test,
              perf.train = mod$perf.train, perf.test = mod$perf.test,
              pca.kern = pca.kern,
              plot = plot.gamma,
              args = args, time = list(t1 = t1, t2 = t2, elapsed = t2 - t1))
  res <- structure(res, class = "caret") # almost
  return(res)
}



######################
### Postprocessing ###
######################

# plot nice confusion matrix (and other model performance plots)
confmat <- function(pred, ref, plot.perf = FALSE, plot.cmat = FALSE, plot.heatmap = TRUE, title = NULL,
                    freq = TRUE, n = TRUE, low = "blue", mid = colorRampPalette(c("blue","orange"))(3)[2], 
                    high = "orange", midpoint = 0.5, filter.rows = NULL, filter.cols = NULL) {
  
  ## Author and Date
  # simon.crameri@usys.ethz.ch, 2021-08-19
  # performance metrics taken from Max Kuhn's caret::confusionMatrix() function
  
  ## Usage
  # pred          character or factor   containing the predicted classes. Can be a matrix or data.frame of ncol(pred) equalling the number of repetitions during e.g. repeated cross-validation.       
  # ref           character or factor   containing the true classes.
  # plot.perf     logical               if TRUE, plots the performance metrics (sensitivity, specificity, precision) [needs ggplot2, caret]
  # plot.cmat     logical               if TRUE, plots the confusion matrix [needs adegenet]
  # plot.heatmap  logical               if TRUE, plots the confusion matrix heatmap [needs ggplot2, scales, tidyr, tibble, dplyr]
  # title         character             suffix for heatmap title
  # freq          logical               if TRUE, scales the heatmap to frequency rather than number of instances
  # n             logical               if TRUE, adds each number of instances to the heatmap
  # low           character             valid R colour (lower end colour in heatmap)
  # mid           character             valid R colour (mid colour in heatmap)
  # high          character             valid R colour (upper end colour in heatmap)
  # midpoint      numeric               definition of midpoint in heatmap colours (depends on <freq>)
  # filter.rows   character             uses grep(filter.rows, invert = TRUE) to filter out rows from the heatmap. Should match levels in pred/ref
  # filter.cols   character             uses grep(filter.cols, invert = TRUE) to filter out cols from the heatmap. Should match levels in pred/ref
  
  ## Check input
  stopifnot(inherits(pred, c("character","factor")),
            inherits(ref, c("character","factor")),
            is.logical(plot.perf), is.logical(plot.cmat), is.logical(plot.heatmap),
            is.logical(freq), is.numeric(midpoint))
  
  ## Helperfunctions
  confusion <- function(df, x, y, midpoint = 0.5, na.value = "#00000000", 
                        xlab = "New identification", ylab = "Previous identification", title.main = "Confusion matrix heatmap", title.sub = "",
                        freq = TRUE, n = TRUE, low = alpha("black", alpha = 0.25), mid = alpha("black", alpha = 0.5), high = alpha("black", alpha = 1),
                        quantile = 0, filter.rows = NULL, filter.cols = NULL) {
    
    # helperfunctions
    table.cont <- function(df, var1, var2, freq = TRUE, quantile = 0) {
      t <- as.matrix(table(df[,var1], df[,var2]))
      t <- matrix(t, ncol = ncol(t), dimnames = dimnames(t))
      if (freq) t <- apply(t, 2, function(x) {x/sum(x, na.rm = TRUE)})
      varsel <- names(which(apply(t, 1, sum, na.rm = TRUE) >= quantile(apply(t, 1, sum, na.rm = TRUE), quantile)))
      t <- t[varsel,]
      return(t)
    }
    var.heatmap <- function(df, low = muted("red"), mid = "white", high = muted("blue"), limits = NULL,
                            midpoint = mean(as.matrix(df)), keyname = "",
                            xlab = FALSE, cex.xlab = 8, col.xlab = 1, ylab = TRUE, cex.ylab = 8, col.ylab = 1,
                            width = 1, height = 1, bg = "white", na.value = "grey50", plot = TRUE) {
      
      ## Arguments
      
      # check dependencies
      needs <- c("tibble","tidyr","dplyr","ggplot2")
      inst <- which(!needs %in% installed.packages())
      if (length(inst) > 0) {
        for (i in inst) {
          toinst <- readline(prompt = paste0("The <", needs[i], "> package is needed. Do you wish to install it? [y/n]: "))
          if (toinst == "y") install.packages(needs[i])
        }
      }
      
      require(ggplot2)
      require(dplyr)
      
      # reshape
      dd <- tibble::as_tibble(df, rownames = NA) %>% 
        tibble::rownames_to_column(var = "Y") %>% 
        tidyr::pivot_longer(cols = -1, names_to = "X", values_to = "Z") %>%
        dplyr::mutate(X = factor(X, levels = colnames(df))) %>%
        dplyr::mutate(Y = factor(Y, levels = rev(rownames(df))))
      
      p <- ggplot(dd, aes(X, Y, fill = Z)) + 
        geom_tile(width = width, height = height) +
        scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = midpoint, na.value = na.value, limits = limits) +
        labs(x = "", y = "", fill = keyname) +
        theme_minimal() +
        theme(axis.text.x = if (xlab) element_text(angle = 90, hjust = 1, vjust = 0.5, size = cex.xlab, colour = col.xlab) else element_blank(), 
              axis.text.y = if (ylab) element_text(colour = col.ylab, size = cex.ylab) else element_blank(),
              axis.ticks = element_blank()) #+
      # scale_x_discrete(position = "top")
      
      if (plot) print(p)
      invisible(p)
    }
    
    # harmonize levels
    zx <- levels(df[,x])[which(table(df[,x]) == 0)]
    zy <- levels(df[,y])[which(table(df[,y]) == 0)]
    df[,x] <- factor(df[,x], levels = levels(df[,x])[!levels(df[,x]) %in% intersect(zx, zy)])
    df[,y] <- factor(df[,y], levels = levels(df[,y])[!levels(df[,y]) %in% intersect(zx, zy)])
    
    # get confusion matrix
    df.tab <- table.cont(df, var1 = y, var2 = x, freq = freq, quantile = quantile)
    df.tab[df.tab %in% c(0, NA, NaN)] <- NA
    
    # filter zero rows and columns
    if (!is.null(filter.rows)) {
      # keep.rows <- apply(df.tab, 1, function(x) !all(is.na(x)))
      keep.rows <- grep(filter.rows, rownames(df.tab), invert = TRUE)
      df.tab <- df.tab[keep.rows,]
    }
    if (!is.null(filter.cols)) {
      # keep.cols <- apply(df.tab, 2, function(x) !all(is.na(x)))
      keep.cols <- grep(filter.cols, colnames(df.tab), invert = TRUE)
      df.tab <- df.tab[,keep.cols]
    }
    
    # plot confusion matrix
    p <- var.heatmap(df = df.tab, keyname = ifelse(freq, "Frequency", "Count"),
                     low = low, mid = mid, high = high, xlab = TRUE,
                     midpoint = midpoint, width = 0.95, height = 0.95, na.value = na.value, plot = FALSE)
    p <- p +
      labs(x = xlab, y = ylab) +
      geom_abline(slope = -1, intercept = nrow(df.tab) + 1, size = 0.35, colour = "black") +
      theme(panel.grid.major = element_line(size = c(0.35,0.25,0.25), linetype = c("solid"), colour = c("black","gray50","gray50"))) +
      ggtitle(label = title.main, subtitle = title.sub) +
      theme(plot.title = element_text(lineheight = .8, size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.7)))
    
    # add text
    if (is.null(filter.cols) & is.null(filter.rows) & n) {
      labs <- as.numeric(table(ref = factor(ref, levels = colnames(df.tab)),
                               pred = factor(pred, levels = rownames(df.tab))))
      labs[labs == 0] <- NA
      p <- p + 
        geom_text(aes(label = labs), na.rm = T)
    }
    
    print(p)
    invisible(p)
  }
  
  ##############################################################################
  
  ## Get confusion matrix/matrices
  ref1 <- ref
  if (any(is.matrix(pred) & ncol(pred) > 1, is.data.frame(pred) & ncol(pred) > 1)) {
    ref2 <- data.frame(array(NA, dim = list(nrow = nrow(pred), ncol = ncol(pred)),
                             dimnames = list(NULL, paste("rep", 1:ncol(pred)))))
    for (i in 1:ncol(ref2)) ref2[,i] <- ref1 ; ref <- ref2 
    confmat <- table(Prediction = factor(pred[,1], levels = levels(ref1)), 
                     Reference = ref[,1])
    for (i in 2:ncol(pred)) {
      ls <- table(Prediction = factor(pred[,1], levels = levels(ref1)), 
                  Reference = ref[,i])
      confmat <- confmat + ls
    }
  } else {
    pred <- factor(pred, levels = levels(ref1))
    confmat <- table(Prediction = pred, Reference = ref1)
  }
  
  ## Calculate overall accuracy/error
  true <- sum(diag(confmat))
  false <- sum(confmat)-true
  error <- paste0(round(100*false/sum(confmat),3), "%")
  
  ## Detailed statistics per class
  refnames <- gsub(" ", "", levels(ref1))
  d.pred <- data.frame(pred)
  summary <- array(NA, dim = c(4*nlevels(ref1) + 2, 5), 
                   dimnames = list(c("Accuracy", "BER", 
                                     paste0("Sensitivity.", refnames),
                                     paste0("Specificity.", refnames),
                                     paste0("Precision.", refnames), 
                                     paste0("BalancedAccuracy.", refnames)),
                                   c("metric", "class", "mean", "N", "sd")))
  
  # Accuracy and BER
  if (ncol(d.pred) == 1) {
    tab <- table(d.pred[,1], ref1)
    Accuracy <- 1 - (sum(tab) - sum(diag(tab))) / sum(tab)
    
    prop.wrong <- sapply(levels(ref1), FUN = function(y) {
      cl.pred <- d.pred[,1][ref1 == y]
      sum(cl.pred != y) / length(cl.pred)
    })
    BER <- mean(prop.wrong)
    
  } else {
    Accuracy <- apply(d.pred, 2, 
                      function(x) {
                        tab <- table(x, ref1)
                        1 - (sum(tab) - sum(diag(tab))) / sum(tab)
                      })
    
    BER <- apply(d.pred, 2,
                 function(x) {
                   prop.wrong <- sapply(levels(ref1), FUN = function(y) {
                     cl.pred <- x[ref1 == y]
                     sum(cl.pred != y) / length(cl.pred)
                   })
                   mean(prop.wrong)
                 })
  }
  
  # Precision, Sensitivity, Specificity
  Precision <- apply(d.pred, 2,
                     function(x) {
                       x <- factor(x, levels = levels(ref1))
                       precision <- numeric()
                       for (i in levels(ref1)) {
                         tab <- table(x, ref1 == i)
                         if (any(ref1 == i)) {
                           A <- tab[i, "TRUE"]
                         } else {A <- 0}
                         B <- tab[i, "FALSE"]
                         prec <- A / (A+B)
                         names(prec) <- i
                         precision <- c(precision, prec)
                         rm(i)
                       }
                       precision
                     })
  
  Sensitivity <- apply(d.pred, 2,
                       function(x) {
                         x <- factor(x, levels = levels(ref1))
                         sensitivity <- numeric()
                         for (i in levels(ref1)) {
                           tab <- table(x, ref1 == i)
                           if (any(ref == i)) {
                             A <- tab[i, "TRUE"]
                             C <- sum(tab[which(levels(ref1) != i), "TRUE"])
                           } else {
                             A <- C <- 0}
                           sens <- A / (A+C)
                           names(sens) <- i
                           sensitivity <- c(sensitivity, sens)
                           rm(i)
                         }
                         sensitivity
                       }) 
  
  Specificity <- apply(d.pred, 2,
                       function(x) {
                         x <- factor(x, levels = levels(ref1))
                         specificity <- numeric()
                         for (i in levels(ref1)) {
                           tab <- table(x, ref1 == i)
                           B <- tab[i, "FALSE"]
                           D <- sum(tab[which(levels(ref1) != i), "FALSE"])
                           spec <- D / (B+D)
                           names(spec) <- i
                           specificity <- c(specificity, spec)
                           rm(i)
                         }
                         specificity
                       })
  
  BalancedAccuracy <- (Sensitivity + Specificity) / 2
  
  metrics <- c("Accuracy","BER", "Sensitivity", "Specificity", "Precision", "BalancedAccuracy")
  single <- c("Accuracy","BER")
  summary <- data.frame(summary)
  for (i in metrics) {
    dat <- get(i)
    xx <- if (i %in% single) i else paste0(i,".", refnames)
    summary[xx, "metric"] <- i
    summary[xx, "class"] <- if (i %in% single) i else levels(ref1)
    summary[xx, "mean"] <- if (i %in% single) mean(dat) else apply(dat, 1, mean)
    summary[xx, "N"] <- if (i %in% single) length(dat) else apply(dat, 1, length)
    summary[xx, "sd"] <- if (i %in% single) sd(dat) else apply(dat, 1, sd)
    rm(i)
  }
  
  ## Save results
  acc <- 1-false/sum(confmat)
  res <- list()
  res[["confmat"]] <- confmat
  res[["summary"]] <- summary
  res[["overall.accuracy"]] <- acc
  res[["overall.accuracy.perc"]] <- paste0(round(100*acc,3), "%")
  res[["overall.error"]] <- false/sum(confmat)
  res[["overall.error.perc"]] <- error
  res[["overall.BER"]] <- summary["BER","mean"]
  res[["overall.BER.perc"]] <- paste0(round(100*summary["BER","mean"], 3), "%")
  
  ## Plot results
  # stats
  # get accuracy CI
  if ("caret" %in% installed.packages()) {
    cstats <- caret::confusionMatrix(pred, ref)$overall # gives CI
  } else {
    cstats <- acc
    names(cstats) <- "Accuracy"
  }
  
  # subtitle
  subtitle <- paste0("Accuracy = ", round(cstats["Accuracy"], 3),
                     " [", round(cstats["AccuracyLower"], 3), ",", round(cstats["AccuracyUpper"], 3), "]",
                     # ", AccuracyLower = ", round(cstats["AccuracyLower"], 3),
                     # ", AccuracyUpper = ", round(cstats["AccuracyUpper"], 3),
                     # ", AccuracyNull = ", round(cstats["AccuracyNull"], 3),
                     ", p-Value = ", round(cstats["AccuracyPValue"], 4),
                     "; Kappa = ", round(cstats["Kappa"], 3),
                     "; BER = ", round(summary["BER","mean"], 3))
  
  # Performance metrics
  if (plot.perf) {
    suppressPackageStartupMessages(require(ggplot2))
    summary[,"class"] <- factor(summary[,"class"], levels = levels(ref))
    summary[,"metric2"] <- factor(summary[,"metric"], levels = unique(summary[,"metric"]))
    gplot1 <- ggplot(summary[!summary$metric %in% single,], aes(mean, class)) +
      geom_errorbarh(aes(xmin = mean - sd, xmax = mean + sd), height = 0.1) +
      geom_point(aes(mean)) +
      # facet_wrap(~class) + # if y is mapped to metric2 instead of class
      facet_wrap(~metric2) +
      xlab(ifelse(any(is.matrix(pred), is.data.frame(pred)), "mean +- sd", "mean")) +
      ylab("") +
      ggtitle(label = paste0("Performance metrics by class"),
              subtitle = subtitle) +
      theme_bw() +
      theme(plot.title = element_text(lineheight = .8, size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.7)))
    
    suppressWarnings(print(gplot1))
    res[["plot.perf"]] <- gplot1
  }
  if (plot.perf & plot.cmat) suppressWarnings(print(last_plot())) # needed to print gplot1 ???
  
  # Confusion matrix (square size proportional to number of instances)
  if (plot.cmat) {
    suppressPackageStartupMessages(require(adegenet))
    table.value(confmat, csize = 1, col.labels = levels(ref1))
    mtext(paste0("Reference (n = ", length(ref), ")"), side = 3, line = -1, outer = TRUE)
    mtext(paste0("Predicted (Accuracy = ", round(acc, 3), ")"), side = 4, line = -1, outer = TRUE)
  }
  
  # Confusion heatmap (colour proportional to number / frequency of instances)
  if (plot.heatmap) {
    gplot2 <- confusion(df = data.frame(pred = pred, ref = ref), x = "ref", y = "pred", 
                        xlab = paste0("Reference (n = ", length(ref), ")"), 
                        ylab = paste0("Predicted (Accuracy = ", round(acc, 3), ")"),
                        low = low, mid = mid, high = high, midpoint = midpoint, freq = freq,
                        title.main = if (is.null(title)) paste0("Confusion matrix heatmap") else paste0("Confusion matrix heatmap (", title, ")"),
                        title.sub = subtitle, filter.rows = filter.rows, filter.cols = filter.cols)
    res[["plot.heatmap"]] <- gplot2
  }
  
  
  ## return results
  invisible(res)
}
