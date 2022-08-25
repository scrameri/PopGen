################################################################################
#### Apply wrappers tidytune() and caret() for streamlined machine learning ####
################################################################################

## Simon Crameri, Aug. 2022
library(tidymodels)
library(discrim)
library(caret)

## Load machine learning wrapper functions
# load caret(), tidytune(), confmat() and some generic plot and print methods
source("https://raw.githubusercontent.com/scrameri/PopGen/main/src/ml.R")


# These wrapper functions enable access to the wide array of classification
# (and regression) methods, accessed via the packages caret (caret::tune)
# and tidymodels (tidymodels::tune_grid) maintained by Max Kuhn.

# The wrapper functions can be used for a more streamlined execution of model
# training (hyperparameter tuning), final model fitting (on complete training
# data), and model evaluation using the confmat() function.

# Look at these websites for lists of available methods accessible via caret()
# - http://topepo.github.io/caret/train-models-by-tag.html
# - http://topepo.github.io/caret/using-your-own-model-in-train.html for details

# Look at these websites for lists of available models accessible via tidytune()
# - https://www.tidymodels.org/find/parsnip/


## Example dataset: iris
str(iris)

## Create a training and test dataset
set.seed(154)
idx <- caret::createDataPartition(y = iris[,"Species"], times = 1, p = 2/3, list = FALSE)[,1]
Xtrain <- iris[idx,]
Xtest <- iris[-idx,]
Ytrain <- iris[idx,"Species"]
Ytest <- iris[-idx,"Species"]

## Tune models with caret()
library(caret)

# caret settings
metric <- "Accuracy"
control <- trainControl(method = "repeatedcv", number = 10, repeats = 1,
                        search = "grid", verboseIter = TRUE)

# random forest
mod <- "rf"
modelLookup(model = mod) # mtry can be tuned

res <- caret(Xtrain = Xtrain[,-5], Ytrain = Ytrain, Xtest = Xtest[,-5], Ytest = Ytest, 
             method = mod, metric = metric, trControl = control,
             tuneGrid = expand.grid(mtry = c(1:4)), ntree = 500) # pass additional (fixed) arguments via ...

confmat(pred = res$pred.test, ref = res$args$Ytest)

# neural network (nnet)
mod <- "nnet"
modelLookup(model = mod) # size and decay can be tuned

res <- caret(Xtrain = Xtrain[,-5], Ytrain = Ytrain, Xtest = Xtest[,-5], Ytest = Ytest, 
             method = mod, metric = metric, trControl = control,
             tuneGrid = expand.grid(size = c(1:4), decay = c(0.001,0.01,0.1,1.10)))

confmat(pred = res$pred.test, ref = res$args$Ytest)



## Tune models with tidytune()
# recipe for preprocessing
rec <- recipe(formula = Species ~ ., data = tibble(Xtrain)) %>%
  step_center(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), num_comp = 4)

# specify cross validation
folds <- vfold_cv(data = rec$template, v = 10, repeats = 1, strata = "Species", pool = 0.1)

# control tuning process
trControl <- control_grid(verbose = TRUE, save_pred = FALSE, save_workflow = TRUE)
metrics <- yardstick::metric_set(accuracy, kap, sens, spec)

# mda::fda flexible discriminant analysis
mod <- parsnip::discrim_linear(mode = "classification", engine = "mda", penalty = tune())
show_model_info(class(mod)[1]) ; mod %>% translate() # the "mda" engine accepts <penalty> to be tuned

# specify model training workflow (preprocessing recipe, model fitting)
workflow <- workflow(preprocessor = rec, spec = mod)
tuneGrid <- expand.grid(penalty = c(0.001,0.01,0.1,1,10,100))

# run tidytune wrapper function
res <- tidytune(Xtrain = Xtrain, Ytrain = Ytrain,
                Xtest = Xtest, Ytest = Ytest,
                workflow = workflow, tuneGrid = tuneGrid,
                folds = folds, trControl = trControl,
                metrics = metrics, metric.best = "accuracy")

confmat(pred = res$pred.test$.pred_class, ref = res$args$Ytest)
