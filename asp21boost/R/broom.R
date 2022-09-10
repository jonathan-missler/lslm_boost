#' @importFrom generics tidy
#' @export

generics::tidy

#' @importFrom generics tidy
#' @importFrom stats coef pnorm vcov
#' @export

tidy.lslm_boost <- function(x, predictors = c("location", "scale"), ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)

  if (length(predictors) > 1) {
    out <- lapply(predictors, function(p) tidy(x, p))
    out <- do.call(rbind, out)
  } else {
    out <- data.frame(
      predictor = predictors,
      term      = names(coef(x, predictors)),
      estimate  = coef(x, predictors),
      row.names = NULL
    )
  }

  out
}


