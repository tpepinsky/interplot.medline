#' @title A Wrapper for Interplot (Holt and Su) to add Median Lines
#'
#' @description This package adds a median line to a standard
#' marginal effects plot (Brambor, Clark, and Golder 2006) created by \code{\link{interplot}} (Solt and Hu 2015).
#'
#' @param m A model object with interaction terms
#' @param var1 The variable for which you wish to calculate marginal effects
#' @param var2 The moderator
#'
#' @return The function returns a \code{\link{ggplot}} object.
#'
#' @examples  interplot.medline(m = fit, var1 = "cyl", var2 = "wt")
#'
#' @seealso \code{\link{interplot}}
#'
#' @references Brambor, Thomas, William Roberts Clark and Matt Golder. 2006. Understanding Interaction Models: Improving Empirical Analyses. \emph{Political Analysis} 14(1), pp. 63-82.
#' @references Solt, Frederick, and Yue Hu. 2015. \code{interplot}: Plot the Coefficients of Variables in Interaction Terms. The Comprehensive R Archive Network (CRAN).
#'
#' @export interplot.medline

interplot.medline <- function(m,
                              var1,
                              var2,
                              ci = .95){

  # check to see if supported model
  if (class(m)[1] == "glm") {
    print("is GLM")
    if (m$family[1] != "binomial")
      stop("Interplot.medline only supports OLS, logit, and probit.")
  }

  # get the median value of the moderator
  if (var2 %in% names(m$coef)) {
    med <- median(eval(parse(text = paste0("m$model$", var2))))
  }
  else
    stop(paste("Model does not include ", var2, "."))

  # get coefficients
  if (var1 %in% names(m$coef)) {
    coef1 <- coef(m)[var1]
  }
  else
    stop(paste("Model does not include ", var1, "."))

  if (paste0(var2,":", var1) %in% names(m$coef)) {
    coef3 <- coef(m)[ paste0(var2,":", var1)]
  }
  else if (paste0(var1,":", var2) %in% names(m$coef)) {
    coef3 <- coef(m)[ paste0(var1,":", var2)]
  }
  else
    stop(paste("Model does not include the interaction of",
               var1, "and", var2, "."))
  print(coef1)
  print(coef3)

  #make the line
  hline <- coef1 + med*coef3

  # get DV
  depvar <- names(m$model)[1]

  # create the graph
  p <- interplot::interplot(m=m, var1 = var1, var2 = var2, ci=ci, hist=TRUE) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept=hline, linetype="dashed") +
    ggplot2::geom_hline(yintercept=0) +
    ggplot2::labs(x = paste("Moderator: ", var2, sep="")) +
    ggplot2::labs(y= paste("Marginal Effect of ",var1," on ", depvar, sep=""))
  return(p)

}

