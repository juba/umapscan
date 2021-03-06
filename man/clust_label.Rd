% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{clust_label}
\alias{clust_label}
\title{Label an umapscan cluster}
\usage{
clust_label(us, id, label)
}
\arguments{
\item{us}{umapscan object}

\item{id}{cluster identifier}

\item{label}{cluster label}
}
\value{
An updated umapscan object (invisibly).
}
\description{
Label an umapscan cluster
}
\examples{
library(dplyr)
iris_num <- iris \%>\% select_if(is.numeric)
us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
us <- clust_compute(us, minPts = 3, eps = 0.5, graph = FALSE)
us <- clust_rename(us, "1", "clust_1")
us <- clust_label(us, "clust_1", "Cluster 1")
us$clusters
}
\seealso{
\code{\link[=clust_rename]{clust_rename()}}
}
