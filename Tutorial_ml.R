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
# (and regression) methods, accessed via tuning with the packages caret
# (caret::tune) and tidymodels (tune::tune_grid) maintained by Max Kuhn.

# The wrapper functions can be used for a more streamlined execution of model
# training (hyperparameter tuning), final model fitting (on complete training
# data), model evaluation, and prediction of new data.

# There are some additional functions which may be useful for machine learning
# - split.data (https://github.com/scrameri/PopGen/wiki/split.data)
# - confmat (pretty confusion matrix)

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

res # or print(res)
summary(res) # has a print method, more is returned invisibly
plot(res) # has some useful arguments, see args(plot.caret)
confmat(pred = res$pred.test, ref = res$args$Ytest)

# neural network (nnet)
mod <- "nnet"
modelLookup(model = mod) # size and decay can be tuned

res2 <- caret(Xtrain = Xtrain[,-5], Ytrain = Ytrain, Xtest = Xtest[,-5], Ytest = Ytest, 
             method = mod, metric = metric, trControl = control,
             tuneGrid = expand.grid(size = c(1:4), decay = c(0.001,0.01,0.1,1.10)))

res2 # or print(res2)
summary(res2) # has a print method, more is returned invisibly
plot(res2) # has some useful arguments, see args(plot.caret)
confmat(pred = res2$pred.test, ref = res2$args$Ytest)



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
res3 <- tidytune(Xtrain = Xtrain, Ytrain = Ytrain,
                 Xtest = Xtest, Ytest = Ytest,
                 workflow = workflow, tuneGrid = tuneGrid,
                 folds = folds, trControl = trControl,
                 metrics = metrics, metric.best = "accuracy")

res3 # or print(res3)
summary(res3) # has a print method, more is returned invisibly
plot(res3) # has some useful arguments, see args(plot.caret)
confmat(pred = res3$pred.test$.pred_class, ref = res3$args$Ytest) # note that tidytune $pred.X are tibbles


## Precit new data
# assume that the test set <res3$args$Xtest> is new data
predict(res3, res3$args$Xtest) # tibble of factor, same as using type = "class"
predict(res3, res3$args$Xtest, type = "raw") # factor
predict(res3, res3$args$Xtest, type = "prob") # posterior probabilities
pred <- tibble(predict(res3, new_data = res3$args$Xtest, type = "class"),
               predict(res3, new_data = res3$args$Xtest, type = "prob"),
               true_class = res3$args$Ytest)
confmat(pred = pred$.pred_class, ref = pred$true_class, plot.perf = TRUE, plot.heatmap = FALSE)
