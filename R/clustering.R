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

compute_clusters <- function(us, parent = "", noise_only = FALSE, eps, minPts, graph = TRUE, alpha = 1, ellipses = TRUE) {

  if (!inherits(us, "umapscan")) stop("`us` must be an object of class umapscan.")

  if (noise_only) {
    ids <- get_noise_ids(us, parent)
  } else {
    ids <- get_ids(us, parent)
  }
  us <- remove_cluster(us, cluster = parent, noise_only = noise_only)

  d_clust <- us$umap %>% slice(ids)
  set.seed(us$seed)
  if (missing(eps)) {
    message("Missing eps, hdbscan performed instead of dbscan")
    db <- dbscan::hdbscan(d_clust, minPts = minPts)
  } else {
    db <- dbscan::dbscan(d_clust, eps = eps, minPts = minPts)
  }

  clust <- db$cluster
  parent_string <- ifelse(parent == "", "", paste0(parent, "_"))
  clust <- paste0(parent_string, clust)
  clust[db$cluster == 0] <- "<Noise>"
  d_clust$cluster <- clust


  for (cl in unique(clust)) {
    if (is.na(cl)) next
    select <- (clust == cl) & !is.na(clust)
    line <- tibble(
      from = parent,
      to = cl,
      n = length(ids[select]),
      ids = list(ids[select])
    )
    us$clusters <- bind_rows(us$clusters, line)
  }

  select <- us$clusters$to != "<Noise>"
  us$clusters$to[select] <- make.unique(us$clusters$to[select])

  if (graph) {
    g <- plot_clusters(us, parent, alpha = alpha)
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

plot_clusters <- function(us, parent = "", noise_inherit_parent = FALSE, alpha = 1, ellipses = TRUE, fixed = FALSE) {

  clust <- get_clusters_membership(us, parent, noise_inherit_parent = noise_inherit_parent)
  if (all(is.na(clust))) stop("No defined clusters in umapscan object.")
  d_clust <- us$umap[!is.na(clust),]
  clust <- clust[!is.na(clust)]
  clust[clust == "<Noise>"] <- NA
  d_clust$cluster <- clust
  color_scale <- qualitative_palette(clust, label = "Cluster")

  g <- ggplot(d_clust, aes(x=x, y=y, color = factor(cluster))) +
    geom_point(size = 1, alpha = alpha) +
    color_scale +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

  if (ellipses) {
    d_ellipses <- d_clust %>% drop_na(cluster)
    g <- g + stat_ellipse(data = d_ellipses, na.rm = TRUE)
  }

  if (fixed) {
    g <- g + coord_fixed()
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

describe_clusters <- function(us, parent = "", type = c("boxplot", "ridges")) {

  type <- match.arg(type)

  clusters <- get_clusters_membership(us, parent, noise_inherit_parent = FALSE)
  clusters[clusters == "<Noise>"] <- NA

  select <- !is.na(clusters)
  d <- us$data %>% dplyr::filter(select)
  d$cluster <- clusters[select]

  d_long <- d %>%
    tidyr::pivot_longer(-cluster) %>%
    dplyr::mutate(
      cluster = factor(cluster)
    )

  fill_scale <- qualitative_palette(d_long$cluster, "Cluster", type = "fill")

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


get_clusters_membership <- function(us, parent = "", noise_inherit_parent = FALSE) {

  if (parent == "<Noise>") {
    stop("Can't get membership starting from a <Noise> node")
  }

  clusters <- rep(NA, nrow(us$data))

  if (nrow(us$clusters) == 0) {
    return(clusters)
  }

  leaves <- get_leaves(us, node = parent)

  for (i in 1:nrow(leaves)) {
    leaf <- leaves[i,]
    if (leaf$to == "<Noise>") {
      if (noise_inherit_parent) {
        clusters[get_noise_ids(us, leaf$from)] <- leaf$from
      } else {
        clusters[get_noise_ids(us, leaf$from)] <- leaf$to
      }
    } else {
      clusters[get_ids(us, leaf$to)] <- leaf$to
    }
  }

  clusters
}



#' Get leaves from a node
#' (not exported)

get_leaves <- function(us, node = "", parent = NA) {

  if (node == "<Noise>" && is.na(parent)) {
    stop("Can't get leaves from a <Noise> node.")
  }

  children <- us$clusters$to[us$clusters$from == node]

  if (length(children) == 0) {
    return(tibble(from = parent, to = node))
  }
  leaves <- tibble(from = character(0), to = character(0))
  for(child in children) {
    leaves <- bind_rows(leaves, get_leaves(us, child, parent = node))
  }
  return(leaves)
}


#' Get ids from a node name
#' (not exported)

get_ids <- function(us, node) {

  if (node == "") {
    return(1:nrow(us$data))
  }

  us$clusters %>%
    filter(to == node) %>%
    pull(ids) %>%
    unlist
}

#' Get ids from a <Noise> child of a node
#' (not exported)

get_noise_ids <- function(us, node) {
  us$clusters %>%
    filter(from == node, to == "<Noise>") %>%
    pull(ids) %>%
    unlist
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

  ids <- get_ids(us, cluster)
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
#' An updated umapscan object (invisibly). If two clusters have the same name after
#' renaming, they are merged together.
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7)
#' rename_cluster(us, "1", "Cluster 1")
#' us

rename_cluster <- function(us, old, new) {

  us$clusters <- us$clusters %>%
    mutate(
      from = if_else(from == old, new, from),
      to = if_else(to == old, new, to),
    ) %>%
    group_by(from, to) %>%
    summarise(
      n = sum(n),
      ids = list(unlist(c(ids)))
    ) %>%
    ungroup()

  us
}


#' Remove a cluster from an umapscan object
#'
#' If the cluster has children, they will be removed too.
#'
#' @param us umapscan object
#' @param cluster label of the cluster to remove
#' @param rm_root if TRUE,
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

remove_cluster <- function(us, cluster, rm_root = FALSE, noise_only = FALSE) {

  if (noise_only) {
    us$clusters <- us$clusters %>% filter(!(from == cluster & to == "<Noise>"))
    return(us)
  }

  from_lines <- us$clusters %>%
    filter(from == cluster) %>%
    select(this_from = from, this_to = to)
  purrr::pwalk(from_lines, function(this_from, this_to) {
    ## Ensure we only remove one <Noise> node
    if (this_to == "<Noise>") {
      us$clusters <<- us$clusters %>%
        filter(!(from == this_from & to == this_to))
    } else {
      us <<- remove_cluster(us, this_to, rm_root = TRUE)
    }
  })

  if (rm_root) {
    to_lines <- us$clusters$to == cluster
    us$clusters <- us$clusters %>% filter(!to_lines)
  }

  us
}


#' @export

umapscan_tree <- function(us) {

  tree <- data.tree::as.Node(us$clusters, mode = "network")

  collapsibleTree::collapsibleTree(
    tree,
    collapse = FALSE,
    tooltip = TRUE,
    attribute = "n",
    inputId = "node",
    inputClickedId = "clicked",
  )

}

#' @export

reinit_clusters <- function(us) {
  us$clusters <- tibble(
    from = character(0),
    to = character(0),
    n = integer(0),
    ids = list()
  )

  us
}


#' @export

collapse_clusters <- function(us, collapsed) {

  for (node in collapsed) {
    us <- remove_cluster(us, node)
  }

  us
}




