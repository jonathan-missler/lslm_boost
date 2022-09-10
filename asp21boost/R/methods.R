#' @export

coef.lslm_boost <- function(object, predictors = c("location", "scale"), ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)

  if (length(predictors) > 1) {
    out <- object$coefficients[predictors]
  } else {
    out <- object$coefficients[[predictors]]
  }

  out
}



#' @export

fitted.lslm_boost <- function(object, predictors = c("location", "scale"), ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)

  if (length(predictors) > 1) {
    out <- object$fitted.values[predictors]
  } else {
    out <- object$fitted.values[[predictors]]
  }

  out
}

#' @importFrom stats dnorm fitted nobs
#' @export

logLik.lslm_boost <- function(object, ...) {
  y <- object$y
  location <- fitted(object, "location")
  scale <- exp(fitted(object, "scale"))

  out <- sum(dnorm(y, location, scale, log = TRUE))

  attr(out, "nobs") <- nobs(object)
  class(out) <- "logLik"

  out
}

#' @importFrom graphics abline plot
#' @importFrom stats fitted qnorm resid
#' @export

plot.lslm_boost <- function(x,
                      xlab = "Fitted values",
                      ylab = "Pearson residuals",
                      ...) {
  plot(
    x = fitted(x, "location"),
    y = resid(x, "pearson"),
    xlab = xlab,
    ylab = ylab,
    ...
  )

  abline(h = qnorm(c(0.025, 0.975)), lty = "dashed")
  abline(h = 0)
  invisible(x)
}

#' @importFrom stats as.formula coef fitted model.matrix predict update
#' @export

predict.lslm_boost <- function(object,
                         newdata = NULL,
                         type = c("link", "response"),
                         predictors = c("location", "scale"),
                         ...) {
  predictors <- match.arg(predictors, several.ok = TRUE)
  type <- match.arg(type)

  if (is.null(newdata)) {
    out <- fitted(object, predictors)

    if (type == "link" && any(predictors == "scale")) {
      out$scale <- log(out$scale)
    }
  } else {
    if (length(predictors) > 1) {
      out <- lapply(predictors, function(p) {
        predict(object, newdata, type, p)
      })

      names(out) <- predictors
    } else {
      formula <- as.formula(object$call[[predictors]])
      #formula <- update(formula, NULL ~ .) #not sure what this does.

      mm <- model.matrix(formula, newdata)
      out <- drop(mm %*% coef(object, predictors))

      if (type == "response" && predictors == "scale") {
        out <- exp(out)
      }
    }
  }

  out
}

#' @importFrom stats coef
#' @export

print.lslm_boost <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat(
    "\nCall:\n",
    paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )

  if (length(coef(x, "location"))) {
    cat("Location coefficients:\n")

    print.default(
      x = format(coef(x, "location"), digits = digits),
      print.gap = 2,
      quote = FALSE
    )
  } else {
    cat("No location coefficients\n")
  }

  cat("\n")

  if (length(coef(x, "scale"))) {
    cat("Scale coefficients:\n")

    print.default(
      x = format(coef(x, "scale"), digits = digits),
      print.gap = 2,
      quote = FALSE
    )
  } else {
    cat("No scale coefficients\n")
  }

  cat("\n")
  invisible(x)
}

#' @importFrom stats qqline qqnorm resid
#' @export

qqnorm.lslm_boost <- function(y,
                        xlab = "Theoretical quantiles",
                        ylab = "Pearson residuals",
                        ...) {
  qqnorm(resid(y, "pearson"), xlab = xlab, ylab = ylab, ...)
  qqline(resid(y, "pearson"))
  invisible(y)
}

#' @importFrom stats fitted
#' @export

residuals.lslm_boost <- function(object, type = c("response", "pearson"), ...) {
  type <- match.arg(type)
  out <- object$residuals

  if (type == "pearson") {
    out <- out / fitted(object, "scale")
  }

  out
}




#' @importFrom stats AIC BIC coef df.residual logLik printCoefmat resid
#' @export

summary.lslm_boost <- function(object,
                         digits = max(3, getOption("digits") - 3),

                         ...) {


  cat(
    "\nCall:\n",
    paste(deparse(object$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )


  if (length(coef(object, "location"))) {
    cat("Location coefficients (identity link function):\n")
    print(coef(object, "location"), digits = digits, ...)
  } else {
    cat("No location coefficients\n")
  }

  cat("\n")

  if (length(coef(object, "scale"))) {
    cat("Scale coefficients (log link function):\n")
    print(coef(object, "scale"), digits = digits, ...)
  } else {
    cat("No scale coefficients\n")
  }

  cat("\n")


  cat(
    "Log-likelihood:",
    format(signif(logLik(object), digits)),
    "\n"
  )



  invisible(object)
}


