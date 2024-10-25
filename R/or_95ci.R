#' Odds Ratios and 95% Confidence Intervals
#'
#' A function that calculates odds ratios and 95% confidence intervals
#' from a vector of coefficient estimates and a vector of standard errors.
#'
#' @param coef A vector of coefficient estimates.
#' @param se A vector of standard errors.
#' @param siglevel The significance level (0.05 for 95% confidence).
#' @param roundto The number of decimal places to round to.
#' @return A string that contains odds ratios and 95% confidence intervals.
#' @author Anna He
#' @examples
#' OR_95CI(coef = c(0.5, -0.3), se = c(0.1, 0.15), siglevel = 0.05, roundto = 2)
#' @export

OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}

