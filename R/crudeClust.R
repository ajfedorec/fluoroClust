
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
      ggplot2::geom_density(data=as.data.frame(fF[, c(channel)]@exprs),
                            ggplot2::aes(x=`BL1-H`, y=..count..),
                            fill="grey",
                            alpha=0.5) +
      ggplot2::geom_density(data=as.data.frame(fF.high[, c(channel)]@exprs),
                            ggplot2::aes(x=`BL1-H`, y=..count..),
                            fill="green",
                            alpha=0.5) +
      ggplot2::xlim(c(0,7))+
      ggplot2::ggtitle(paste("Crude clustered at ", threshold, "\n", flowCore::identifier(fF), sep=""))+
      ggplot2::theme_classic()+
      ggplot2::theme(strip.text.x = ggplot2::element_text(size=12),
            strip.background = ggplot2::element_rect(colour="white"),
            axis.text = ggplot2::element_text(size=10),
            axis.text.x = ggplot2::element_text(angle=-40, vjust = 0.5),
            axis.title = ggplot2::element_text(size=12),
            plot.title = ggplot2::element_text(size=14),
            text = ggplot2::element_text(family='Garamond'))


    ggplot2::ggsave(filename = paste(filename, "_clusters.png", sep=""), plot=plt, width = 7, height = 4, units = "in", dpi = 600)
    # grDevices::dev.off()
    print(paste("Plotting cluster ", flowCore::identifier(fF)))
  }
  return(newF)
}
