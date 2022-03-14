#' Compute descriptive statistics for the numeric columns of a data frame.
#' @param x The numerical variable to describe
#' @param ... Optional. Columns in the data frame
#' @return A data frame with descriptive statistics.
#' @examples
#' \dontrun{
#' descr(iris$Sepal.Length)
#' }

descr <- function(x){

  stopifnot("Your input vectormust be numeric" = is.numeric(x))
  range <- paste( c("range:"), round(range(x)[1],1), "to", round(range(x)[2],1))
  mean <- paste( c("mean"), round(mean(x),1), sep = ": ")
  median <- paste( c("median"), round(median(x),1), sep = ": ")
  IQR <- paste( c("IQR"), round(IQR(x),1), sep = ": ")
  box <- paste( c('box'), round(quantile(x, 0.25),1), "-", round(quantile(x, 0.75),1) )
  result <- list(range, mean, median, IQR, box)
  return(result)
}
