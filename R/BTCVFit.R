#' BTCVFit
#'
#' CV (Adaptive) Boosting Tree Model Object.
#'
#' @description These are objects representing CV fitted boosting trees.
#'
#' @return a list of \code{\link{BTFit}} objects with each element corresponding to a specific BT fit on a particular fold
#'
#' @section Structure : The following components must be included in a legitimate \code{BTCVFit} object.
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
#' @name BTCVFit
#'
NULL
