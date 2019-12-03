#' Compute DBSCAN clusters from a umapscan object
#'
#' This function runs DBSCAN with the specified arguments to compute new clusters
#' on a umapscan object. Only point which don't have an already validated cluster
#' are taken into account.
#'
#' @param us umapscan object
#' @param eps `eps` argument passed to [dbscan::dbscan()]
#' @param minPts `minPts` argument passed to [dbscan::dbscan()]
#' @param graph if TRUE, display a plot of the computed clusters
#' @param alpha point transparency for clusters plot
#'
#' @return
#' Returns an updated `umapscan` object, and optionally displays a clusters plot.
#'
#' @seealso
#' [new_umapscan()], [describe_clusters()], [get_cluster()], [label_cluster()]
#'
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1)
#' compute_clusters(us, minPts = 10, eps = 0.72, alpha = 1)
#'
#' @importFrom dbscan dbscan

compute_clusters <- function(us, eps, minPts, graph = TRUE, alpha = 0.5) {

  if (!inherits(us, "umapscan")) stop("`us` must be an object of class umapscan.")

  d_clust <- us$umap %>% filter(!us$cluster_validated)
  db <- dbscan::dbscan(d_clust, eps = eps, minPts = minPts)

  d_clust$cluster <- db$cluster
  d_clust$cluster[d_clust$cluster == "0"] <- NA

  if (n_distinct(d_clust$cluster) <= 12) {
    color_scale <- scale_color_brewer("Cluster", palette = "Paired", na.value = "grey50")
  } else {
    color_scale <- ggsci::scale_color_d3(palette = "category20", name = "Cluster", na.value = "white")
  }

  g <- ggplot(d_clust, aes(x=x, y=y, color = factor(cluster))) +
    geom_point(size = 1, alpha = alpha) +
    color_scale +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

  print(g)

  us$cluster[!us$cluster_validated] <- d_clust$cluster

  us

}


#' Describe current clusters of an umapscan object
#'
#' Only the not already validated clusters are taken into account.
#'
#' @param us umapscan object to describe clusters
#' @param type plot type, either `"boxplot"` or `"ridges"`
#'
#' @seealso
#' [compute_clusters()], [get_cluster()], [label_cluster()]
#'
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1)
#' us <- compute_clusters(us, minPts = 10, eps = 0.72, alpha = 1)
#' describe_clusters(us)

describe_clusters <- function(us, type = c("boxplot", "ridges")) {

  type <- match.arg(type)

  select <- !us$cluster_validated & !is.na(us$cluster)
  d <- us$data %>% dplyr::filter(select)
  d$cluster <- us$cluster[select]

  if (dplyr::n_distinct(d$cluster) <= 12) {
    fill_scale <- scale_fill_brewer("Cluster", palette = "Paired", na.value = "grey50")
  } else {
    fill_scale <- ggsci::scale_fill_d3(palette = "category20", name = "Cluster", na.value = "white")
  }

  d_long <- d %>%
    tidyr::pivot_longer(-cluster) %>%
    dplyr::mutate(
      cluster = factor(cluster, levels = sort(as.numeric(unique(cluster))))
    )

  if (type == "boxplot") {
    g <- ggplot(d_long, aes(x = cluster, y = value, fill = cluster)) +
      geom_boxplot(outlier.shape = NA) +
      fill_scale +
      facet_wrap(~name, scales = "free_y")
  }

  if (type == "ridges") {
    g <- ggplot(d_long, aes(x = value, y = cluster, fill = cluster)) +
      ggridges::geom_density_ridges(alpha = .7) +
      fill_scale +
      facet_wrap(~name, scales = "free_x") +
      guides(fill = guide_legend(override.aes = list(alpha = 1)))
  }

  g

}





#' Get an umapscan cluster data
#'
#' @param us umapscan object
#' @param clust cluster to retrieve
#'
#' @seealso
#' [compute_clusters()], [describe_clusters()], [label_cluster()]
#'
#' @return
#' 1 data frame with the `data` and `data_sup` variables of the cluster observations
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1)
#' us <- compute_clusters(us, minPts = 10, eps = 0.72, alpha = 1)
#' get_cluster(us, 1)

get_cluster <- function(us, clust) {

  clust <- as.character(clust)
  d <- dplyr::bind_cols(us$data_sup, us$data, cluster = us$cluster)
  d %>% dplyr::filter(cluster == clust)

}



#' Label and validate an umapscan cluster
#'
#' @param us umapscan object
#' @param clust cluster identifier
#' @param label label to attribute to cluster
#'
#' @seealso
#' [compute_clusters()], [describe_clusters()], [get_cluster()]
#'
#' @return
#' A updated umapscan object
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1)
#' us <- compute_clusters(us, minPts = 10, eps = 0.72, alpha = 1)
#' us <- label_cluster(us, 1, "Cluster 1")
#' us

label_cluster <- function(us, clust, label) {
  clust <- as.character(clust)
  select <- us$cluster == clust
  us$cluster[select] <- label
  us$cluster_validated[select] <- TRUE

  us
}
