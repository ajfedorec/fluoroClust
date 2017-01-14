#' A function to cluster flow cytometry data by one fluorescence dimension in to "on" and "off" populations.
#'
#' Cluster data in a \code{\link[flowCore]{flowFrame}} using the \code{\link[flowClust]{flowClust}}
#' package. Given a fluorescence channel and, optionally, prior values for the on and off
#' peak locations, determines how many clusters there are and produces information on the populations.
#'
#' @param fF a \code{\link[flowCore]{flowFrame}} to cluster.
#' @param channel the fluorescence channel on which to cluster.
#' @param prior.max the expected location of the "on" peak.
#' @param prior.min the expected location of the "off" peak.
#' @param do.plot a Boolean flag to determine whether to produce plots showing the trimming of each flowFrame. Defaults to \code{FALSE}.
#'
#' @return a \code{\link{data.frame}} containing \code{num_samples}, the
#' number of samples clustered, \code{max_clust_mean}, the mean value of fluoresence
#' in the "on" cluster, and \code{max_clust_prop}, the proportion of the total
#' population that is "on".
#' @export
#'
#' @examples
iclClust <- function( fF, channel, prior.max, prior.min, do.plot ){
  ## Remove any NaN, NA, Inf or -Inf values from the flowFrame
  fF <- flowCore::Subset(fF, as.logical(is.finite(flowCore::exprs(fF[, channel]))))

  ## calculate clusters for K=1 and K=2
  flowClust.res <- flowClust::flowClust(fF, varNames=c(channel), K=1:2, criterion="ICL");

  ## get the results for the K with the best ICL
  flowClust.res <- flowClust.res[[flowClust.res@index]]
  est <- flowClust::getEstimates(flowClust.res, fF)

  ## If there is only 1 cluster, is it plasmid-free or plasmid-bearing
  if(length(est$locationsC) == 1){
    ## distance of the peak from the base fluorescence
    dist.fM <- (est$locationsC - prior.min)^2
    dist.fP <- (est$locationsC - prior.max)^2

    ## if the peak is closer to the plasmid-bearing base fluorescence...
    if(dist.fM > dist.fP){
      newF <- data.frame(num_samples=length(flowClust.res@label),
                         max_clust_mean=est$locationsC,
                         max_clust_prop=est$proportions)
    }
    ## otherwise assume it is plasmid-free
    else{
      newF <- data.frame(num_samples=length(flowClust.res@label),
                         max_clust_mean=est$locationsC,
                         max_clust_prop=0)
    }
  }
  ## otherwise take the proportion of the peak with the highest mean fluorescence
  else{
    newF <- data.frame(num_samples=length(flowClust.res@label),
                       max_clust_mean=est$locationsC[which.max(est$locationsC)],
                       max_clust_prop=est$proportions[which.max(est$locationsC)])
  }
  if(do.plot){
    filename <- substr(flowCore::keyword(fF, "FILENAME"), 1, nchar(flowCore::keyword(fF, "FILENAME"))-4)
    grDevices::png(paste(filename, "_clusters.png", sep=""))
    flowClust::hist(flowClust.res, data=fF, xlim=c(0,7),
                    main=paste("ICL clustered\n", flowCore::identifier(fF)))
    grDevices::dev.off()
    print(paste("Plotting cluster ", flowCore::identifier(fF)))
  }

  return(newF)
}
