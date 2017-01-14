
crudeClust <- function( fF, channel, threshold, do.plot ){
  ## Remove any NaN, NA, Inf or -Inf values from the flowFrame
  fF <- flowCore::Subset(fF, as.logical(is.finite(flowCore::exprs(fF[, channel]))))

  fF.high <- flowCore::Subset(fF, as.logical(flowCore::exprs(fF[, channel]) > 3.5))

  newF <- data.frame(num_samples=nrow(fF),
                     max_clust_mean=mean(flowCore::exprs(fF.high[, channel])),
                     max_clust_prop=nrow(fF.high)/nrow(fF))

  if(do.plot){
    filename <- substr(flowCore::keyword(fF, "FILENAME"), 1, nchar(flowCore::keyword(fF, "FILENAME"))-4)
    # grDevices::png(paste(filename, "_clusters.png", sep=""))
    plt <- ggplot2::ggplot() +
      ggplot2::geom_area(data=as.data.frame(fF[, c(channel)]@exprs),
                aes(x=`BL1-H`, y=..count..),
                alpha=0.5,
                stat = "bin") +
      ggplot2::geom_area(data=as.data.frame(fF.high[, c(channel)]@exprs),
                aes(x=`BL1-H`, y=..count..),
                fill="green",
                alpha=0.5,
                stat = "bin") +
      ggtitle(paste("Crude clustered\n", flowCore::identifier(fF)))

    ggplot2::ggsave(filename = paste(filename, "_clusters.png", sep=""), plot=plt)
    # grDevices::dev.off()
    print(paste("Plotting cluster ", flowCore::identifier(fF)))
  }
  return(newF)
}
