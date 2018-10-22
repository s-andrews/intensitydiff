#' Intensity difference plot
#'
#' This function takes the output from the intensity_difference
#' function and draws a number of diagnostic plots to help to 
#' evaluate the results of the analysis
#' 
#' The function will split the current plot area into 4 sections
#' and will plot 4 separate graphs:
#' 
#' 1. The distribution of difference values from the data.  This 
#'    should ideally form a single coherent distribution with a 
#'    median of zero.
#'    
#' 2. The relationship between average intensity and standard
#'    deviation. The entire point of this test is that there 
#'    should be linkage between these two values, so if you don't
#'    see a relationship here then this test is not appropriate
#'    to use.  Normally you would expect that the standard deviation
#'    would fall as the average intensity rises.
#'    
#' 3. An MA plot showing the difference between datasets plotted
#'    against the average intensity.  The colouring of points on
#'    the plot reflects whether they were found to be signficant
#'    by the statistical test which was run.  The colouring is
#'    grey (not significant), blue (significant by raw p-value), 
#'    but not after multiple testing correction, red (significant
#'    after multiple testing correction). You should always see 
#'    the blue points occupying the outer 5% of the distribution.
#'    You may or may not see any red points depending on the 
#'    behaviour of your data.
#'    
#' 4. A Z-score MA plot.  This is the same as 3 but using the 
#'    normalised z-score differences instead of the raw differences.
#'    You would expect that the width of the cloud of points should
#'    be roughly equal across this plot, and there shouldn't be an
#'    intensity dependent effect visible across the plot.
#'
#' @param intensity.diff.data The output data frame from the intensity.difference function
#' @export
#' @examples
#' intensity_diff_plot()

intensity.diff.plot <- function (
  intensity.diff.data
) {
  
  par(mfrow=c(2,2))
  
  .plot.diff.distribution(intensity.diff.data)
  .plot.mean.vs.sd(intensity.diff.data)
  .plot.ma(intensity.diff.data)
  .plot.zscore.ma(intensity.diff.data)
  
  
}

.plot.zscore.ma <- function(intensity.diff.data) {
  
  plot.limit <- max(abs(intensity.diff.data$z.score))
  
  not.signif.logical <- intensity.diff.data$p.value > 0.05
  raw.signif.logical <- intensity.diff.data$p.value <= 0.05 & intensity.diff.data$fdr.value > 0.05
  corr.signif.logical <- intensity.diff.data$fdr.value <= 0.05 
  
  plot(
    intensity.diff.data$average.intensity[not.signif.logical],
    intensity.diff.data$z.score[not.signif.logical],
    pch=19,
    cex=0.5,
    col="grey",
    las=1,
    ylim=c(0-plot.limit,plot.limit),
    xlim=range(intensity.diff.data$average.intensity),
    xlab="Average intensity",
    ylab="Z-score difference",
    main="Z-score difference MA plot hits"
  )
  
  points (
    intensity.diff.data$average.intensity[raw.signif.logical],
    intensity.diff.data$z.score[raw.signif.logical],
    pch=19,
    cex=0.5,
    col="blue2"
  )
  
  points (
    intensity.diff.data$average.intensity[corr.signif.logical],
    intensity.diff.data$z.score[corr.signif.logical],
    pch=19,
    cex=0.5,
    col="red2"
  )
  
  abline(h=0)
  
  legend(
    "topright",
    legend = c("NS","p<0.05","FDR<0.05"),
    fill=c("grey","blue2","red2")
  )
  
}

.plot.ma <- function(intensity.diff.data) {

  plot.limit <- max(abs(intensity.diff.data$difference))
  
  not.signif.logical <- intensity.diff.data$p.value > 0.05
  raw.signif.logical <- intensity.diff.data$p.value <= 0.05 & intensity.diff.data$fdr.value > 0.05
  corr.signif.logical <- intensity.diff.data$fdr.value <= 0.05 
   
  plot(
    intensity.diff.data$average.intensity[not.signif.logical],
    intensity.diff.data$difference[not.signif.logical],
    pch=19,
    cex=0.5,
    col="grey",
    las=1,
    ylim=c(0-plot.limit,plot.limit),
    xlim=range(intensity.diff.data$average.intensity),
    xlab="Average intensity",
    ylab="Raw difference",
    main="Raw difference MA plot hits"
  )

  points (
    intensity.diff.data$average.intensity[raw.signif.logical],
    intensity.diff.data$difference[raw.signif.logical],
    pch=19,
    cex=0.5,
    col="blue2"
  )
  
  points (
    intensity.diff.data$average.intensity[corr.signif.logical],
    intensity.diff.data$difference[corr.signif.logical],
    pch=19,
    cex=0.5,
    col="red2"
  )

  abline(h=0)
  
  legend(
    "topright",
    legend = c("NS","p<0.05","FDR<0.05"),
    fill=c("grey","blue2","red2")
  )
    
}


.plot.mean.vs.sd <- function(intensity.diff.data) {
  
  data <- intensity.diff.data[,c("average.intensity","local.sd")]
  
  data[order(data$average.intensity),] -> data
  
  plot(
    data$average.intensity,
    data$local.sd,
    type="l",
    xlab="Average intensity",
    ylab="Local Standard Deviation",
    las=1,
    main="Variation in StDEV vs average intensity"
  )

}


.plot.diff.distribution <- function (intensity.diff.data) {
  
  # We'll set the limits so that we cover 95% of the 
  # data.  We're only interested in the general trends
  # so it doesn't matter if we miss out a few points
  # at the edge
  
  diffs <- intensity.diff.data$difference
  
  sort(abs(diffs)) -> abs.diffs
  
  plot.limit <- abs.diffs[(as.integer)(length(abs.diffs)*0.95)]
  
  plot(
    density(diffs[diffs<=plot.limit & diffs >= (0-plot.limit)]),
    las=1,
    xlim=c(0-plot.limit,plot.limit),
    xlab="Difference between conditions",
    main="Distribution of difference values"
    )
  
  abline(v=0,lwd=2,col="red")
  
}
