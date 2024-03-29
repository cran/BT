#' BTFit
#'
#' Boosting Tree Model Object.
#'
#' @description These are objects representing fitted boosting trees.
#'
#' @return
#' \item{BTInit}{an object of class \code{BTInit} containing the initial fitted value \code{initFit}, the initial \code{training.error} and the initial \code{validation.error} if any.}
#'
#' \item{BTErrors}{an object of class \code{BTErrors} containing the vectors of errors for each iteration performed (excl. the initialization). More precisely, it contains the \code{training.error},
#' \code{validation.error} if \code{train.fraction}<1 and the \code{oob.improvement} if \code{bag.fraction} < 1.
#' Moreover, if a cross-validation approach was performed, a vector of cross-validation errors \code{cv.error} as a function of boosting iteration is also stored in this object.}
#'
#' \item{BTIndivFits}{an object of class \code{BTIndivFits} containing the list of each individual tree fitted at each boosting iteration.}
#'
#' \item{distribution}{the Tweedie power (and so the distribution) that has been used to perform the algorithm. It will currently always output 1.}
#'
#' \item{var.names}{a vector containing the names of the explanatory variables.}
#'
#' \item{response}{the name of the target/response variable.}
#'
#' \item{w}{a vector containing the weights used.}
#'
#' \item{seed}{the used seed, if any.}
#'
#' \item{BTData}{if \code{keep.data=TRUE}, an object of class \code{BTData} containing the \code{training.set} and \code{validation.set} (can be NULL if not used). These data frames are reduced
#' to the used variables, that are the response and explanatory variables. Note that in case of cross-validation, even if \code{keep.data=TRUE} the folds will not be kept. In fact, only the data
#' frames related to the original fit (i.e. on the whole training set) will be saved.}
#'
#' \item{BTParams}{an object of class \code{BTParams} containing all the (Adaptive) boosting tree parameters. More precisely, it contains the \code{ABT}, \code{train.fraction},
#' \code{shrinkage}, \code{interaction.depth}, \code{bag.fraction}, \code{n.iter}, \code{colsample.bytree} and \code{tree.control} parameter values.}
#'
#' \item{keep.data}{the \code{keep.data} parameter value.}
#'
#' \item{is.verbose}{the \code{is.verbose} parameter value.}
#'
#' \item{fitted.values}{the training set fitted values on the score scale using all the \code{n.iter} (and initialization) iterations.}
#'
#' \item{cv.folds}{the number of cross-validation folds. Set to 1 if no cross-validation performed.}
#'
#' \item{call}{the original call to the \code{BT} algorithm.}
#'
#' \item{Terms}{the \code{model.frame} terms argument.}
#'
#' \item{folds}{a vector of values identifying to which fold each observation is in. This argument is not present if there is no cross-validation. On the other hand, it corresponds
#' to \code{folds.id} if it was initially defined by the user.}
#'
#' \item{cv.fitted}{a vector containing the cross-validation fitted values, if a cross-validation was performed. More precisely, for a given observation, the prediction will be furnished by the cv-model
#' for which this specific observation was out-of-fold. See \code{\link{predict.BTCVFit}} for more details.}
#'
#' @section Structure : The following components must be included in a legitimate \code{BTFit} object.
#'
#' @author Gireg Willame \email{gireg.willame@@gmail.com}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}.
#'
#' @references M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |: GLMs and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries ||: Tree-Based Methods and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |||: Neural Networks and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2022). \strong{Response versus gradient boosting trees, GLMs and neural networks under Tweedie loss and log-link}.
#' Accepted for publication in \emph{Scandinavian Actuarial Journal}.
#'
#' M. Denuit, J. Huyghe and J. Trufin (2022). \strong{Boosting cost-complexity pruned trees on Tweedie responses: The ABT machine for insurance ratemaking}.
#' Paper submitted for publication.
#'
#' M. Denuit, J. Trufin and T. Verdebout (2022). \strong{Boosting on the responses with Tweedie loss functions}. Paper submitted for publication.
#'
#' @keywords methods
#' @name BTFit
#'
NULL
