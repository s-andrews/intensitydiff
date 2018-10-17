#' Intensity difference test
#'
#' This function calculates an intensity corrected z-score
#' statistic for a matched pair of numeric vectors. For each
#' point a local sub-sampling of the data is performed where
#' a set of points with the most similar mean values across
#' the two datasets are collected.  The distribution of
#' differences for these selected points is then modelled with
#' a normal distribution and a z-score value for the point
#' being tested is then calculated, along with the p.value
#' corresponding to that z-score.  The absolute difference
#' and FDR corrected p-values are also reported.
#'
#' @param values.1 A numeric vector of values
#' @param values.2 A numeric vector of values
#' @param window.proportion What proportion of the data will be used to create each local model - Default 0.01
#' @export
#' @examples
#' intensity.difference()

intensity.difference <- function (
  values.1,
  values.2,
  window.proportion=0.01,
  ) {

  if (!is.numeric(values.1)) {
    stop("The data in values.1 was not numeric")
  }

  if (!is.numeric(values.2)) {
    stop("The data in values.2 was not numeric")
  }


  if (length(values.1) != length(values.2)) {
    stop("The two vectors passed to intensity.difference must be the same length")
  }

  return.frame <- data.frame(values.1,values.2)

  average.values <- (values.1+values.2)/2

  order(average.values) -> sorted.indices

  order(sorted.indices) -> reverse.lookup

  window.size <- as.integer(length(values.1)*window.proportion)
  half.window.size <- as.integer(window.size/2)

  if (half.window.size < 1) {
    stop(paste("Sample size is too small when using window.proportion of",window.proportion))
  }


  sapply(1:length(values.1), function(x) {
    start <- reverse.lookup[x]-half.window.size
    if (start < 0) start <- 0
    end <- start+window.size
    if (end > length(values.1)) {
      end <- length(values.1)
      start <- end-500
    }

    local.diffs <- values.1[sorted.indices[start:end]]-values.2[sorted.indices[start:end]]

    # We assume a mean of 0 and calculate the sd
    sqrt(mean(local.diffs*local.diffs)) -> local.sd

    # Now we work out the p.value for the value we're actually
    # looking at in the context of this distibution
    pnorm(values.1[x]-values.2[x],mean=0,sd=local.sd) -> local.p

    if (local.p >0.5) local.p <- (1 - local.p)

    return (local.p)

  }
  ) -> return.frame$p.value

  return.frame$corr.p.value <- p.adjust(return.frame$p.value,method="fdr")

}
