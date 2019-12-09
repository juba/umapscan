#' Compute DBSCAN clusters from a umapscan object
#'
#' This function runs DBSCAN with the specified arguments to compute new clusters
#' on a umapscan object. Only point which don't have an already validated cluster
#' are taken into account.
#'
#' @param us umapscan object
#' @param parent name of the parent cluster
#' @param eps `eps` argument passed to [dbscan::dbscan()]
#' @param minPts `minPts` argument passed to [dbscan::dbscan()]
#' @param graph if TRUE, display a plot of the computed clusters
#' @param alpha point transparency for clusters plot
#' @param ellipses if TRUE, plot confidence ellipses around clusters
#'
#' @return
#' Returns an updated `umapscan` object, and optionally displays a clusters plot.
#'
#' @seealso
#' [new_umapscan()], [describe_clusters()], [get_cluster()], [rename_cluster()],
#' [plot_clusters()], [get_clusters_membership()], [remove_cluster()]
#'
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7)
#' compute_clusters(us, minPts = 3, eps = 0.45, alpha = 1, parent = "3")
#'
#' @importFrom dbscan dbscan

compute_clusters <- function(us, parent, eps, minPts, graph = TRUE, alpha = 1, ellipses = TRUE) {

  if (!inherits(us, "umapscan")) stop("`us` must be an object of class umapscan.")

  if (missing(parent)) {
    node <- us$clusters
  } else {
    node <- data.tree::FindNode(us$clusters, parent)
  }
  ids <- node$ids

  d_clust <- us$umap %>% slice(ids)
  set.seed(us$seed)
  db <- dbscan::dbscan(d_clust, eps = eps, minPts = minPts)

  clust <- db$cluster
  parent_string <- ifelse(missing(parent), "", paste0(parent, "_"))
  clust <- paste0(parent_string, clust)
  clust[db$cluster == 0] <- NA
  d_clust$cluster <- clust


  for (cl in unique(clust)) {
    if (is.na(cl)) next
    .tmp <- node$AddChild(cl)
    select <- (clust == cl) & !is.na(clust)
    .tmp$ids <- ids[select]
    .tmp$n <- length(.tmp$ids)
  }

  if (graph) {
    g <- plot_clusters(us, parent, alpha)
    print(g)
  }

  us
}



#' Plot clusters of an umapscan object
#'
#' @param us umapscan object
#' @param parent name of the parent cluster
#' @param alpha point transparency for clusters plot
#' @param ellipses if TRUE, plot confidence ellipses around clusters
#'
#' @seealso
#' [compute_clusters()], [describe_clusters()]
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7, graph = FALSE)
#' plot_clusters(us, alpha = 1)
#' us <- compute_clusters(us, minPts = 3, eps = 0.45, graph = FALSE, parent = "3")
#' plot_clusters(us, alpha = 0.5, ellipses = FALSE, parent = "3")
#' plot_clusters(us)
#'
#' @import ggplot2

plot_clusters <- function(us, parent, alpha = 1, ellipses = TRUE) {

  clust <- get_clusters_membership(us, parent)
  if (all(is.na(clust))) stop("No defined clusters in umapscan object.")
  d_clust <- us$umap[!is.na(clust),]
  clust <- clust[!is.na(clust)]
  d_clust$cluster <- clust

  color_scale <- qualitative_palette(clust, label = "Cluster")

  g <- ggplot(d_clust, aes(x=x, y=y, color = factor(cluster))) +
    geom_point(size = 1, alpha = alpha) +
    color_scale +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

  if (ellipses) {
    g <- g + stat_ellipse()
  }

  g
}


#' Describe clusters of an umapscan object
#'
#' @param us umapscan object to describe clusters
#' @param type plot type, either `"boxplot"` or `"ridges"`
#' @param parent name of the parent cluster
#'
#' @seealso
#' [compute_clusters()], [get_cluster_data()], [get_clusters_membership()],
#' [rename_cluster()], [remove_cluster()]
#'
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7)
#' describe_clusters(us)
#' us <- compute_clusters(us, minPts = 3, eps = 0.45, parent = "3")
#' describe_clusters(us, type = "ridge")
#' describe_clusters(us, parent = "3")

describe_clusters <- function(us, parent, type = c("boxplot", "ridges")) {

  type <- match.arg(type)

  clusters <- get_clusters_membership(us, parent)

  select <- !is.na(clusters)
  d <- us$data %>% dplyr::filter(select)
  d$cluster <- clusters[select]

  if (dplyr::n_distinct(d$cluster) <= 12) {
    fill_scale <- scale_fill_brewer("Cluster", palette = "Paired", na.value = "grey50")
  } else {
    fill_scale <- ggsci::scale_fill_d3(palette = "category20", name = "Cluster", na.value = "white")
  }

  d_long <- d %>%
    tidyr::pivot_longer(-cluster) %>%
    dplyr::mutate(
      cluster = factor(cluster)
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


#' Get cluster membership for each observation of an umapscan object
#'
#' The clusters taken into account are all the "leaves" of the object
#' clusters tree. If a `parent` is specified, only the leaves of its
#' subtree are taken.
#'
#' @param us umapscan object to describe clusters
#' @param parent name of the parent cluster
#'
#' @seealso
#' [compute_clusters()], [get_cluster_data()], [rename_cluster()], [remove_cluster()]
#'
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7)
#' get_clusters_membership(us)
#' us <- compute_clusters(us, minPts = 3, eps = 0.45, parent = "3")
#' get_clusters_membership(us)
#' get_clusters_membership(us, parent = "3")


get_clusters_membership <- function(us, parent) {

  clusters <- rep(NA, nrow(us$data))

  if (missing(parent)) {
    node <- us$clusters
    if (is.null(node$children)) return(clusters)
  } else {
    node <- data.tree::FindNode(us$clusters, parent)
  }

  ids <- node$Get('ids', filterFun = data.tree::isLeaf)

  purrr::iwalk(ids, function(id, name) {
    clusters[id] <<- name
  })

  clusters
}


#' Get an umapscan cluster data
#'
#' @param us umapscan object
#' @param cluster cluster to retrieve
#'
#' @seealso
#' [compute_clusters()], [describe_clusters()], [rename_cluster()]
#'
#' @return
#' 1 data frame with the `data` and `data_sup` variables of the cluster observations
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7)
#' get_cluster_data(us, "1")

get_cluster_data <- function(us, cluster) {

  node <- data.tree::FindNode(us$clusters, cluster)
  ids <- node$ids

  d <- dplyr::bind_cols(us$data_sup, us$data)
  d %>% dplyr::slice(ids) %>% mutate(umpascan_cluster = cluster)

}



#' Rename an umapscan cluster
#'
#' @param us umapscan object
#' @param old current cluster identifier
#' @param new new cluster label
#'
#' @seealso
#' [compute_clusters()], [describe_clusters()]
#'
#' @return
#' An updated umapscan object (invisibly). Note that the original umapscan object is
#' modified in place.
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7)
#' rename_cluster(us, "1", "Cluster 1")
#' us

rename_cluster <- function(us, old, new) {

  node <- data.tree::FindNode(us$clusters, old)
  node$name <- new

  invisible(us)
}


#' Remove a cluster from an umapscan object
#'
#' If the cluster has children, they will be removed too.
#'
#' @param us umapscan object
#' @param cluster label of the cluster to remove
#'
#' @seealso
#' [compute_clusters()], [describe_clusters()]
#'
#' @return
#' An updated umapscan object (invisibly). Note that the original umapscan object is
#' modified in place.
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7)
#' us <- compute_clusters(us, parent = "3" ,minPts = 3, eps = 0.45)
#' remove_cluster(us, "3_1")
#' us
#' remove_cluster(us, "3")
#' us

remove_cluster <- function(us, cluster) {
  parent_node <- data.tree::FindNode(us$clusters, cluster)$parent
  parent_node$RemoveChild(cluster)

  invisible(us)
}


#' @export

umapscan_tree <- function(us) {

  collapsibleTree::collapsibleTree(
    us$cluster,
    collapse = FALSE,
    tooltip = TRUE,
    attribute = "n",
    inputId = "node",
    inputClickedId = "clicked",
  )

}

