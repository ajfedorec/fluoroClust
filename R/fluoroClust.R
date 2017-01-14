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
fluoroClust <- function( fF, clust.type="ICL", channel="BL1-H", prior.max=5, prior.min=3, crude.threshold=3.5, do.plot=F ){
  if(clust.type == "ICL"){
    newF <- iclClust(fF, channel, prior.max, prior.min, do.plot)
  }
  else if(clust.type=="crude"){
    newF <- crudeClust(fF, channel, crude.threshold, do.plot)
  }
  return(newF)
}
