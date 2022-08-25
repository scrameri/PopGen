# tidytune wrapper function for streamlined machine learning using various workflows and model specifications
tidytune <- function(Xtrain, Ytrain, Xtest, Ytest, workflow, tuneGrid, folds,
                     trControl, metrics, metric.best = "accuracy",
                     seed.train = NULL, seed.final = NULL,
                     plot.tune = TRUE, plot.roc = FALSE, ...) {
  
  # author: Simon Crameri, sfcrameri@gmail.com, Aug 2022, confusionMatrix function and dependencies taken from Max Kuhn's caret package.
  
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
  ## info
  # call            matched call to tidytune()
  # mode            "classification" or "regression" mode
  # fform           functional form, e.g. "discrim_linear"
  # engine          engine (package) used for training, e.g. "mda"
  # func            <package>::<function> (a.k.a. pck_fun) used for training
  # nX              number of predictors
  # nSplit          tibble with number of samples in train, test, total
  # nTune           number of hyperparameter combinations tried, equals nrow(tuneGrid)
  # notes           tibble with notes printed during model training
  # args            list of function arguments used (for reproducibility)
  # time            list with starting time ($t1), finish time ($t2), elapsed time ($elapsed), and information on parallel computation
  # 
  ## model tuning results
  # train           object of class "tune_results", output from tune_grid()
  # bestTune        tibble with best hyperparameter combination, including model index (.config)
  # 
  ## classification model fitted on complete training set, and predicted classes
  # mod             object of class "workflow" but fitted to complete Xtrain
  # pred.train      tibble with .pred_class (predicted class), .pred_<classX> (posterior probability for all classes X) for training set
  # pred.test       tibble with .pred_class (predicted class), .pred_<classX> (posterior probability for all classes X) for test set 
  # 
  ## classification model performance metrics  
  # perf.train      list with performance metrics, as outputted by caret::confusionMatrix() for training set
  # perf.test       list with performance metrics, as outputted by caret::confusionMatrix() for test set
  # tidyperf.train  tibble with performance metrics as outputted by
  # tidyperf.test   tibble with performance metrics as outputted by
   
  ## Examples
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
  
  # plot tidytune results
  plot.tidytune <- function(tidytune, order = NULL,
                            x.log10 = TRUE, y.log10 = FALSE, mode = "alpha",
                            point.size = 1, point.alpha = 0.6, line.alpha = 0.6,
                            plot = TRUE) {
    
    require(ggplot2)
    
    ## Get components
    tuneGrid <- tidytune$args$tuneGrid
    met_train <- if (is_tibble(tidytune$perf.train)) {
      tidytune$perf.train
    } else {
      tidytune$tidyperf.train
    }
    bestTune <- tidytune$bestTune
    pkg_fun <- tidytune$func
    
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
      ggtitle(paste0("Hyperparameter tuning: ", sub("_", "::", pkg_fun)))
    
    if (plot) print(p)
    invisible(p)
  }
  
  # print tidytune
  print.tidytune <- function(tidytune) {
    
    perf.train <- if (is_tibble(tidytune$perf.train)) {
      tidytune$perf.train
    } else {
      tidytune$tidyperf.train
    }
    perf.test <- if (is_tibble(tidytune$perf.test)) {
      tidytune$perf.test
    } else {
      tidytune$tidyperf.test
    }
    
    cat("///////  tidytune  ///\n")
    mprec <- tidytune$mod$pre$actions$recipe$recipe
    mspec <- tidytune$mod$fit$actions$model$spec
    print(mprec)
    cat("\n\n")
    print(mspec)
    
    ntrain <- tidytune$nSplit %>% dplyr::filter(set == "train") %>% dplyr::select("n") %>% as.numeric()
    ntest <- tidytune$nSplit %>% dplyr::filter(set == "test") %>% dplyr::select("n") %>% as.numeric()
    ntot <- tidytune$nSplit %>% dplyr::filter(set == "total") %>% dplyr::select("n") %>% as.numeric()
    
    ptrn <- paste0("# training samples: ", stringr::str_pad(ntrain, width = 9))
    ptst <- paste0("# test samples:     ", stringr::str_pad(ntest, width = 9))
    ptot <- paste0("# total samples:    ", stringr::str_pad(ntot, width = 9))
    ppar <- paste0("# parameters:       ", stringr::str_pad(tidytune$nX, width = 9))
    phyp <- paste0("# hyper-par config.:", stringr::str_pad(tidytune$nTune, width = 9))
    nhyp <- paste(names(tidytune$args$tuneGrid), collapse = " ; ")
    acc.test <-  perf.test %>% dplyr::filter(.metric == "accuracy") %>% dplyr::select(".estimate")
    res <- list(spec = mspec,
                n = list(train = as.numeric(ntrain), test = ntest, total = ntot),
                p = list(nparam = as.numeric(tidytune$nX), nhyper = as.numeric(tidytune$nTune)),
                bestTune = as.data.frame(tidytune$bestTune %>% select(!.config)),
                accuracy = as.numeric(acc.test))
    
    cat("> n                             > p\n")
    cat(paste(ptrn, ppar, sep = "   "), "\n")
    cat(paste(ptst, phyp, sep = "   "), "\n")
    cat(paste(ptot, nhyp, sep = "   "),"\n")
    
    cat("\n> performance\n")
    cat(paste0("# test set accuracy:", round(as.numeric(stringr::str_pad(acc.test, width = 9)), 4)), "\n")
    invisible(res)
  }
  
  ## Set parameters
  rec <- workflow$pre$actions$recipe$recipe
  mod <- workflow$fit$actions$model$spec
  
  y <- filter(rec$var_info, role == "outcome")$variable
  fform <- class(mod)[1]
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
  res <- list(call = match.call(), mode = mode, fform = fform, engine = engine, func = pkg_fun,
              train = fit_train, nX = ncol(fit_final$.workflow[[1]]$pre$mold$predictors),
              nSplit = tibble(data.frame(set = names(nTT), n = nTT)),
              nTune = nrow(tuneGrid), bestTune = bestTune, 
              mod = fit_final$.workflow[[1]], plot = try(plot(res), silent = TRUE),
              notes = fit_final$.notes,
              pred.train = pred.train, pred.test = pred.test,
              perf.train = perf.train, perf.test = perf.test,
              tidyperf.train = met_train, tidyperf.test = met_final,
              args = args, time = list(t1 = t1, t2 = t2, elapsed = t2 - t1,
                                       parallel = try(c(foreach::getDoParRegistered()), silent = T),
                                       name = try(c(foreach::getDoParName()), silent = T),
                                       ncpu = try(foreach::getDoParWorkers(), silent = T)))

  res <- structure(res, class = "tidytune")
  
  ## Plot results
  if (totune & plot.tune & !inherits(try(plot(res), silent = TRUE), "try-error")) {print(plot(res))}
  if (totune & plot.roc & !inherits(roc, "try-error")) {print(autoplot(roc))}
  
  ## Return
  invisible(res)
}
