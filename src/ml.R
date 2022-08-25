# tidytune wrapper function for streamlined machine learning using various tidymodels workflows and model specifications
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

# tidytune plot methods: plot.tidytune
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

summary.tidytune <- function(res) {

  require(yardstick)
  
  rec <- res$mod$pre$actions$recipe$recipe
  mod <- res$mod$fit$actions$model$spec
  tuneGrid <- res$args$tuneGrid
  perf.train <- if (is_tibble(res$perf.train)) {
    res$perf.train
  } else {
    res$tidyperf.train
  }
  perf.test <- if (is_tibble(res$perf.test)) {
    res$perf.test
  } else {
    res$tidyperf.test
  }
  
  y <- dplyr::filter(rec$var_info, role == "outcome")$variable
  hpar <- colnames(tuneGrid)
  fform <- class(mod)[1]
  engine <- mod$engine
  transl <- parsnip::translate(mod)
  pkg_fun <- paste(transl$method$fit$func, collapse = "_")
  
  metric.best <- res$args$metric.best
  mfun <- ifelse(attr(eval(parse(text = metric.best[1])),"direction") == "maximize", max, min)
  met_meta <- c(".metric",".estimator","mean","n","std_err",".config")
  
  tuned <- apply(tuneGrid, 2, function(x) {paste(range(x), collapse = ":")})
  tuned <- gsub(" ", "", paste0(paste(paste(names(tuned), tuned, sep = "["), collapse = "];"), "]"))
  
  best <- res$bestTune %>% dplyr::select(-any_of(met_meta))
  best <- paste0(paste(paste(names(best), best, sep = "["), collapse = "];"), "]")
  
  if (inherits(res$train, "tune_results")) {
    acc.train <- res$train %>% 
      tune::show_best(metric = res$args$metric.best, n = 1) %>%
      dplyr::select(any_of(c(".metric","mean","n","std_err",".set")))
  } else {
    acc.train <- res$train$.metrics %>% 
      dplyr::filter(.metric == res$args$metric.best) %>%
      dplyr::filter(mean == mfun(mean)) %>%
      dplyr::select(any_of(c(".metric","mean","n","std_err",".set"))) %>%
      "["(1,)
  }
  acc.train <- acc.train %>% dplyr::rename(!!paste0("Train.", res$args$metric.best) := mean)
  
  acc.test <- perf.test %>% dplyr::select(c(".metric",".estimate"))
  acc.test.head <-t(acc.test)[1,] ; acc.test <- as.numeric(t(acc.test)[2,])
  acc.test <- array(data = acc.test, dim = c(1,length(acc.test)), dimnames = list(NULL, acc.test.head))
  acc.test <- tibble::as_tibble(acc.test) %>% dplyr::rename_all(list(function(x) paste0("Test.", x)))
  
  dres <- 
    tibble::tibble(.rows = 1) %>%
    dplyr::mutate(method = paste(res$fform, sub("_", "::", res$func)),
           tuned = tuned,
           best = best) %>%
    dplyr::bind_cols(acc.train) %>%
    dplyr::bind_cols(acc.test) %>%
    dplyr::mutate(ntrain = res$nSplit %>% dplyr::filter(set == "train") %>% dplyr::select("n") %>% as.numeric(),
           nvar = res$nX,
           nclass = rec$template %>% dplyr::select(all_of(y)) %>% unlist() %>% nlevels(),
           nhypercomb = unlist(res$nTune),
           elapsed = c(res$time$delta, res$time$elapsed),
           ncpu = res$time$ncpu)
  return(structure(dres, class = c("summary.tidytune", class(dres))))
}

predict.tidytune <- function(tidytune, new_data, ...) {
  
  # show_model_info(tidytune$fform) # returns NULL
  predict(tidytune$mod, new_data, ...)
  
}

plot.roc <- function(predictions) {
  require(ggplot2)
  require(yardstick)
  y <- names(predictions)[grep("^[.]", names(predictions), invert = TRUE)]
  predicted <- levels(droplevels(predictions[[y]]))
  roc <- try(predictions %>%
               roc_curve(truth = !!y,
                         estimate = !!(paste(paste0(".pred_", predicted)))),
             silent = TRUE)
  autoplot(roc)
}



# caret wrapper function for streamlined machine learning using various models as in caret::tune_grid
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
              args = args, time = list(t1 = t1, t2 = t2, delta = t2 - t1))
  res <- structure(res, class = "caret")
  invisible(res)
}

# plot caret object
plot.caret <- function(res) {
  plot(res$mod)
}

                                                                     # plot nice confusion matrix (3 options)
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
                                     paste0("Precision.", refnames), 
                                     paste0("Sensitivity.", refnames),
                                     paste0("Specificity.", refnames),
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
  
  metrics <- c("Accuracy","BER", "Precision", "Sensitivity", "Specificity", "BalancedAccuracy")
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
                     ", AccuracyLower = ", round(cstats["AccuracyLower"], 3),
                     ", AccuracyUpper = ", round(cstats["AccuracyUpper"], 3),
                     ", AccuracyNull = ", round(cstats["AccuracyNull"], 3),
                     ", AccuracyPValue = ", round(cstats["AccuracyPValue"], 3),
                     ", Kappa = ", round(cstats["Kappa"], 3),
                     ", BER = ", round(summary["BER","mean"], 3))
  
  # Performance metrics
  if (plot.perf) {
    suppressPackageStartupMessages(require(ggplot2))
    summary[,"class"] <- factor(summary[,"class"], levels = levels(ref))
    
    gplot1 <- ggplot(summary[!summary$metric %in% single,], aes(mean, metric)) +
      geom_errorbarh(aes(xmin = mean - sd, xmax = mean + sd), height = 0.1) +
      geom_point(aes(mean)) +
      facet_wrap(~class) +
      xlab(ifelse(any(is.matrix(pred), is.data.frame(pred)), "mean +- sd", "mean")) +
      ylab("performance metric") +
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
