## ---- echo=FALSE--------------------------------------------------------------
# write("TMPDIR = 'C:\\Users\\Gireg Willame\\Desktop\\TMP'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
# knitr::opts_chunk$set(
#    fig.path = "c:/Users/Gireg Willame/Desktop/TMP/Figures"
# )

## -----------------------------------------------------------------------------
library(BT)

## -----------------------------------------------------------------------------
set.seed(4)
n <- 50000

Gender <- factor(sample(c("male","female"),n,replace=TRUE))
Age <- sample(c(18:65),n,replace=TRUE)
Split <- factor(sample(c("yes","no"),n,replace=TRUE))
Sport <- factor(sample(c("yes","no"),n,replace=TRUE))

lambda <- 0.1*ifelse(Gender=="male",1.1,1)
lambda <- lambda*(1+1/(Age-17)^0.5)
lambda <- lambda*ifelse(Sport=="yes",1.15,1)

ExpoR <- runif(n)

Y <- rpois(n, ExpoR*lambda)
Y_normalized <- Y/ExpoR

dataset <- data.frame(Y,Gender,Age,Split,Sport,ExpoR, Y_normalized)

## -----------------------------------------------------------------------------
BT_algo <- BT(formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
              data = dataset,
              ABT = F,
              n.iter = 300,
              train.fraction = 0.8,
              interaction.depth = 4,
              shrinkage = 0.01,
              bag.fraction = 0.5,
              colsample.bytree = 3,
              keep.data = T,
              is.verbose = F,
              cv.folds = 3,
              folds.id = NULL,
              n.cores = 1,
              weights = ExpoR,
              seed = 44)

## -----------------------------------------------------------------------------
# 'Fitting parameters
BT_algo$call
BT_algo$distribution
BT_algo$BTParams
BT_algo$keep.data
BT_algo$is.verbose
BT_algo$seed
BT_algo$cv.folds # #used folds
head(BT_algo$folds, 10) # folds created.
# Name of weights, response and explnatory variables used
BT_algo$w
BT_algo$response
BT_algo$var.name

## -----------------------------------------------------------------------------
# Tweedie with intercept only.
BT_algo$BTInit$initFit 
# Init training error
BT_algo$BTInit$training.error
# Init validation error
BT_algo$BTInit$validation.error

## -----------------------------------------------------------------------------
head(BT_algo$BTData$training.set)
head(BT_algo$BTData$validation.set, 5)

## -----------------------------------------------------------------------------
# Obtained fitting results at the end - Full BT and CV. 
head(BT_algo$fitted.values)
head(BT_algo$cv.fitted) # cv fitted.
# Errors computed.
head(BT_algo$BTErrors$training.error)
head(BT_algo$BTErrors$validation.error)
head(BT_algo$BTErrors$oob.improvement)
head(BT_algo$BTErrors$cv.error)

## -----------------------------------------------------------------------------
BT_algo$BTIndivFits[[1]] # First tree in the expansion
# One can also access to the usual tree outputs.
head(BT_algo$BTIndivFits[[1]]$frame)

## -----------------------------------------------------------------------------
optimal_perf_validation <- BT_perf(BT_algo, method='validation')
optimal_perf_oob <- BT_perf(BT_algo, method='OOB', oobag.curve = TRUE, overlay = TRUE)
optimal_perf_cv <- BT_perf(BT_algo, method='cv')
optimal_perf_best_guest <- BT_perf(BT_algo, plot.it = FALSE)

## -----------------------------------------------------------------------------
summary(BT_algo)

## -----------------------------------------------------------------------------
# Predict (response scale) using all trees up to best validation iteration. 
head(predict(BT_algo, n.iter = optimal_perf_validation, type = 'response'), 10)
# Predict (link scale) using all trees up the best iteration OOB/CV.
head(predict(BT_algo, newdata = dataset, n.iter = c(optimal_perf_oob, optimal_perf_cv), type = 'link'), 10) 
# Predict using only the 40th tree.
head(predict(BT_algo, n.iter = 40, type = 'response', single.iter = TRUE), 10) 

## -----------------------------------------------------------------------------
print(BT_algo)

## -----------------------------------------------------------------------------
BT_algo <- BT(formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
              data = dataset,
              ABT = F,
              n.iter = 300,
              train.fraction = 0.8,
              interaction.depth = 4,
              shrinkage = 0.01,
              bag.fraction = 0.5,
              colsample.bytree = 3,
              keep.data = T,
              is.verbose = F,
              cv.folds = 1,
              folds.id = NULL,
              n.cores = 1,
              weights = ExpoR,
              seed = 44)

## -----------------------------------------------------------------------------
BT_algo_contd <- BT_more(BT_algo, new.n.iter = 100, seed = 404)
# See parameter, predict, ...
BT_algo_contd$BTParams$n.iter
head(predict(BT_algo_contd, n.iter = 350, type='link'), 10)

