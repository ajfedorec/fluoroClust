#' A function to cluster flow cytometry data by one fluorescence dimension in to "on" and "off" populations.
#'
#' Cluster data in a \code{\link[flowCore]{flowFrame}} using the \code{\link[flowClust]{flowClust}}
#' package. Given a fluorescence channel and, optionally, prior values for the on and off
#' peak locations, determines how many clusters there are and produces information on the populations.
#'
#' @param fF a \code{\link[flowCore]{flowFrame}} to cluster.
#' @param channel the fluorescence channel on which to cluster.
#' @param threshold
#' @param do.plot a Boolean flag to determine whether to produce plots showing the trimming of each flowFrame. Defaults to \code{FALSE}.
#'
#' @return a \code{\link{data.frame}} containing \code{num_samples}, the
#' number of samples clustered, \code{max_clust_mean}, the mean value of fluoresence
#' in the "on" cluster, and \code{max_clust_prop}, the proportion of the total
#' population that is "on".
#' @export
#'
#' @examples
mindensityClust <- function(fF, channel, threshold, do.plot){
  ## Remove any NaN, NA, Inf or -Inf values from the flowFrame
  fF <- flowCore::Subset(fF, as.logical(is.finite(flowCore::exprs(fF[, channel]))))

  g <- openCyto::mindensity(fF, channel = channel)

  pb.clust <- flowCore::Subset(fF, g)

  newF <- data.frame(num_samples = flowCore::nrow(fF),
                     max_clust_mean = mean(flowCore::exprs(pb.clust[, channel])),
                     max_clust_prop = flowCore::nrow(pb.clust) / flowCore::nrow(fF))

  if (do.plot) {
    plt <- ggplot2::autoplot(fF, channel) +
      ggcyto::geom_gate(g) +
      ggplot2::theme_classic() +
      ggplot2::theme(strip.text = ggplot2::element_blank(),
                     strip.background = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(fill = NA, linetype = 1),
                     text = ggplot2::element_text(size = 8))

    filename <- substr(flowCore::keyword(fF, "FILENAME"), 1, nchar(flowCore::keyword(fF, "FILENAME")) - 4)
    ggplot2::ggsave(filename = paste(filename, "_mindensity_clusters.pdf", sep = ""),
                    plot = plt, height = 4, width = 4, units = "in")
  }

  return(newF)
}
