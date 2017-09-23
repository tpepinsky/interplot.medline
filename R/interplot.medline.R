#' @title A Wrapper for Interplot to add Median Lines
#'
#' @description This package adds a median line to a standard
#' marginal effects plot
#'
#' @param m
#' @param var1
#' @param var2
#'
#' @return NULL
#'
#' @examples  interplot.medline(m = mpg, var1 = "cyl", var2 = "wt")
#'
#' @export interplot.medline

interplot.medline <- function(m,
                              var1,
                              var2){

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
  p <- interplot::interplot(m=m, var1 = var1, var2 = var2, hist=TRUE) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept=hline, linetype="dashed") +
    ggplot2::geom_hline(yintercept=0) +
    ggplot2::labs(x = paste("Moderator: ", var2, sep="")) +
    ggplot2::labs(y= paste("Marginal Effect of ",var1," on ", depvar, sep=""))
  return(p)

}


