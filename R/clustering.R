#' Compute DBSCAN clusters from a umapscan object
#'
#' This function runs DBSCAN with the specified arguments to compute new clusters
#' on a umapscan object. Only points which don't have an already validated cluster
#' are taken into account.
#'
#' @param us umapscan object
#' @param parent name of the parent cluster
#' @param noise_only only compute clusters for current 'Noise' points
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
#' [new_umapscan()], [describe_clusters()], [get_cluster_data()], [rename_cluster()],
#' [plot_clusters()], [get_clusters_membership()]
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.5)
#' compute_clusters(us, minPts = 3, eps = 0.45, alpha = 1, parent = "3")
#'
#' @importFrom dbscan dbscan hdbscan

compute_clusters <- function(us, parent = "", noise_only = FALSE, eps, minPts, graph = TRUE, alpha = 1, ellipses = TRUE) {

  if (!inherits(us, "umapscan")) stop("`us` must be an object of class umapscan.")

  if (noise_only) {
    members <- get_noise_members(us, parent)
    us <- remove_noise_cluster(us, parent)
  } else {
    members <- get_members(us, parent)
    us <- remove_cluster(us, parent)
  }



  d_clust <- us$umap %>% slice(members)
  set.seed(us$seed, kind = "default", normal.kind = "default", sample.kind = "default")
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

  if (parent == "") {
    level <- 1
  } else {
    level <- us$clusters$level[us$clusters$id == parent] + 1
  }

  for (cl in unique(clust)) {
    if (is.na(cl)) next
    select <- (clust == cl) & !is.na(clust)
    line <- tibble::tibble(
      parent = parent,
      id = cl,
      n = length(members[select]),
      members = list(members[select]),
      level = level,
      label = ""
    )
    us$clusters <- dplyr::bind_rows(us$clusters, line)
  }

  select <- us$clusters$id != "<Noise>"
  us$clusters$id[select] <- make.unique(us$clusters$id[select])

  if (graph) {
    g <- plot_clusters(us, parent, alpha = alpha, fixed = TRUE)
    print(g)
  }

  us
}




#' Get cluster membership for each observation of an umapscan object
#'
#' The clusters taken into account are all the "leaves" of the object
#' clusters tree. If a `parent` is specified, only the leaves of its
#' subtree are taken.
#'
#' @param us umapscan object to describe clusters
#' @param parent name of the parent cluster
#' @param max_level get membership at most this level deep
#' @param noise_inherit_parent if TRUE, 'Noise' points are given their parent cluster
#'   membership
#' @param if TRUE, use label when possible instead of identifier as cluster names
#'
#' @seealso
#' [compute_clusters()], [get_cluster_data()], [rename_cluster()]
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.5)
#' get_clusters_membership(us)
#' us <- compute_clusters(us, minPts = 3, eps = 0.45, parent = "3")
#' get_clusters_membership(us)
#' get_clusters_membership(us, parent = "3")


get_clusters_membership <- function(us, parent = "", max_level,
  noise_inherit_parent = FALSE, labels = TRUE) {

  if (parent == "<Noise>") {
    stop("Can't get membership starting from a <Noise> node")
  }

  if (missing(max_level)) {
    tree <- us$clusters
  } else {
    tree <- us$clusters %>%
      filter(.data$level <= max_level)
  }

  clusters <- rep(NA, nrow(us$data))

  if (nrow(us$clusters) == 0) {
    return(clusters)
  }

  leaves <- get_leaves(tree, node = parent)

  for (i in 1:nrow(leaves)) {
    leaf <- leaves[i,]
    if (leaf$id == "<Noise>") {
      if (noise_inherit_parent) {
        clusters[get_noise_members(us, leaf$parent)] <- leaf$parent
      } else {
        clusters[get_noise_members(us, leaf$parent)] <- leaf$id
      }
    } else {
      clusters[get_members(us, leaf$id)] <- leaf$id
    }
  }

  ## If labels, replace ids with their labels
  if (labels) {
    cluster_labels <- us$clusters$label[match(clusters, us$clusters$id)]
    clusters <- ifelse(is.na(cluster_labels) | cluster_labels == "", clusters, cluster_labels)
  }

  clusters[clusters == ""] <- "<Noise>"

  clusters
}



#' Get leaves from a node
#'
#' @param tree tree object, such as a `clusters` element of a `umapscan` object
#' @param node node to find leaves from
#' @param parent optional parent of `node` (should not be used directly, only for recursive call).
#'
#' `parent` is used during computation to differentiate different 'Noise' nodes.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble

get_leaves <- function(tree, node = "", parent = NA) {

  if (node == "<Noise>" && is.na(parent)) {
    stop("Can't get leaves from a <Noise> node.")
  }

  children <- tree$id[tree$parent == node]

  if (length(children) == 0) {
    return(tibble::tibble(parent = parent, id = node))
  }
  leaves <- tibble::tibble(parent = character(0), id = character(0))
  for(child in children) {
    leaves <- dplyr::bind_rows(leaves, get_leaves(tree, child, parent = node))
  }
  return(leaves)
}


#' Get members from a node name
#'
#' @param us an umapscan object
#' @param id cluster id to get members from
#'
#' @import dplyr

get_members <- function(us, id) {

  if (id == "") {
    return(1:nrow(us$data))
  }

  if (id == "<Noise>") {
    stop("Can't get members from a <Noise> node.")
  }

  us$clusters %>%
    filter(.data$id == .env$id) %>%
    pull(.data$members) %>%
    unlist
}

#' Get members from a 'Noise' child of a node
#'
#' @param us an umapscan object
#' @param node node to get child 'Noise' members from
#'
#' @import dplyr

get_noise_members <- function(us, node) {

  if (node == "<Noise>") {
    stop("Can't get members from a <Noise> node.")
  }

  us$clusters %>%
    filter(.data$parent == node, .data$id == "<Noise>") %>%
    pull(.data$members) %>%
    unlist
}


#' Get an umapscan cluster data
#'
#' @param us umapscan object
#' @param id cluster id to retrieve
#'
#' @seealso
#' [compute_clusters()], [describe_clusters()], [rename_cluster()]
#'
#' @return
#' 1 data frame with the `data` and `data_sup` variables of the cluster observations
#' @export
#' @import dplyr
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.5)
#' get_cluster_data(us, "1")

get_cluster_data <- function(us, id) {

  id <- as.character(id)

  if (id == "<Noise>") {
    stop("Can't get data for a <Noise> cluster.")
  }

  members <- get_members(us, id)
  d <- dplyr::bind_cols(us$data_sup, us$data)
  d %>%
    dplyr::slice(members) %>%
    dplyr::mutate(umapscan_cluster = id)

}



#' Rename an umapscan cluster identifier
#'
#' @param us umapscan object
#' @param old current cluster identifier
#' @param new new cluster identifier
#'
#' @seealso
#' [label_cluster()]
#'
#' @return
#' An updated umapscan object (invisibly). If two clusters have the same identifier after
#' renaming, they are merged together.
#'
#' @export
#' @import dplyr
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.5)
#' rename_cluster(us, "1", "Cluster 1")
#' us

rename_cluster <- function(us, old, new) {

  if (old == "<Noise>") {
    stop("Can't rename a <Noise> cluster.")
  }

  ## Test for same new name with another parent
  if (new != "<Noise>" && new %in% us$clusters$id) {
    parent_old <- us$clusters$parent[us$clusters$id == old]
    parent_new <- us$clusters$parent[us$clusters$id == new]
    if (parent_old != parent_new) {
      stop("Can't rename a cluster with the same name as another cluster with another parent.")
    }
  }

  us$clusters <- us$clusters %>%
    mutate(
      parent = if_else(.data$parent == old, new, .data$parent),
      id = if_else(.data$id == old, new, .data$id),
    ) %>%
    group_by(.data$parent, .data$id, .data$level) %>%
    summarise(
      n = sum(n),
      members = list(unlist(c(.data$members)))
    ) %>%
    ungroup()

  us
}


#' Label an umapscan cluster
#'
#' @param us umapscan object
#' @param id cluster identifier
#' @param label cluster label
#'
#' @seealso
#' [rename_cluster()]
#'
#' @return
#' An updated umapscan object (invisibly).
#'
#' @export
#' @import dplyr
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.5, graph = FALSE)
#' us <- rename_cluster(us, "1", "clust_1")
#' us <- label_cluster(us, "clust_1", "Cluster 1")
#' us$clusters


label_cluster <- function(us, id, label) {

  id <- as.character(id)

  if (label %in% us$clusters$label) {
    warning("A cluster already exists with the same label.")
  }

  us$clusters$label[us$clusters$id == id] <- label

  us
}


#' Remove a cluster from an umapscan object
#'
#' If the cluster has children, they will be removed too.
#'
#' @param us umapscan object
#' @param id id of the cluster to remove
#' @param rm_root if TRUE, also remove the root cluster node. Otherwise, only remove
#'   its children (should not be used directly, only for recursive call).
#'
#' @return
#' An updated umapscan object.
#'
#' @importFrom purrr pwalk
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.5)
#' us <- compute_clusters(us, parent = "3" ,minPts = 3, eps = 0.45)
#' remove_cluster(us, "3_1")
#' us
#' remove_cluster(us, "3")
#' us
#'
#' @export

remove_cluster <- function(us, id, rm_root = FALSE) {

  if (id == "<Noise>") {
    stop("Can't remove a <Noise> cluster.")
  }

  parent_lines <- us$clusters %>%
    filter(.data$parent == .env$id) %>%
    select(this_parent = .data$parent, this_id = .data$id)

  purrr::pwalk(parent_lines, function(this_parent, this_id) {
    ## Ensure we only remove one <Noise> node
    if (this_id == "<Noise>") {
      us$clusters <<- us$clusters %>%
        filter(!(.data$parent == this_parent & .data$id == this_id))
    } else {
      us <<- remove_cluster(us, this_id, rm_root = TRUE)
    }
  })

  if (rm_root) {
    id_lines <- us$clusters$id == id
    us$clusters <- us$clusters %>% filter(!id_lines)
  }

  us
}


#' Remove a 'Noise' cluster from an umapscan object
#'
#' @param us umapscan object
#' @param parent parent of the 'Noise' cluster to remove

remove_noise_cluster <- function(us, parent) {

  if (parent == "<Noise>") {
    stop("Can't remove a <Noise> cluster from a <Noise> parent.")
  }

  us$clusters <- us$clusters %>%
    filter(!(.data$parent == .env$parent & .data$id == "<Noise>"))

  return(us)
}



#' Reinit all clusters of an umapscan object
#'
#' @param us umapscan object
#'
#' @return
#' An updated umapscan object with an empty clusters element.
#'
#' @export
#' @importFrom tibble tibble

reinit_clusters <- function(us) {
  us$clusters <- tibble::tibble(
    parent = character(0),
    id = character(0),
    n = integer(0),
    members = list(),
    label = character(0)
  )

  us
}


#' Collapse a clusters tree
#'
#' @param us an umapscan object
#' @param collapsed list of nodes to collapse
#'
#' @return
#' An updated umapscan object with `collapsed` nodes children removed.
#'
#' @export

collapse_clusters <- function(us, collapsed) {

  for (node in collapsed) {
    us <- remove_cluster(us, node)
  }

  us
}




