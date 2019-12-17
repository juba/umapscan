#' Compute UMAP embeddings from a numerical data frame
#'
#' @param d a data frame of numeric variables
#' @param n_neighbors `n_neighbors` argument passed to [uwot::umap()]
#' @param min_dist `min_dist` argument passed to [uwot::umap()]
#' @param metric `metric` argument passed to [uwot::umap()]
#' @param scale scaling to be applied to the variables. Value passed to [uwot::umap()].
#' @param seed integer seed passed to [base::set.seed()] before umap embeddings for
#'   reproductibility
#' @param data_sup a data.frame of supplementary variables not used for UMAP
#'   embeddings but for results visualisation
#' @param ... other arguments passed to [uwot::umap()]
#'
#' @return
#' A list of class `umapscan` with the following components :
#' - `umap` : results of UMAP embeddings
#' - `data` : original numerical dataset
#' - `data_sup` : supplementary dataset
#' - `clusters` : a [data.tree::data.tree()] object of clusters
#' - `call` : function call
#' - `seed` : the seed used for random numbers for all umap and dbscan operations
#'
#' @seealso [uwot::umap()], [plot.umapscan()]
#'
#' @export
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' new_umapscan(iris_num, data_sup = iris_sup)

new_umapscan <- function(
  d,
  n_neighbors = 15,
  min_dist = 0.01,
  metric = "euclidean",
  scale = FALSE,
  seed,
  data_sup = NULL,
  ...
) {

  if (!inherits(d, "data.frame")) stop("d must be a numerical data frame.")
  if (!is.null(data_sup)) {
    if (!inherits(data_sup, "data.frame")) stop("data_sup must be a data frame.")
    if (nrow(data_sup) != nrow(d)) stop("d and data_sup must have the same number of rows.")
  }

  d_comp <- d

  if (missing(seed)) { seed <- round(runif(1) * 100000) }

  set.seed(seed)
  umap <- uwot::umap(
    d_comp,
    n_neighbors = n_neighbors,
    min_dist = min_dist,
    metric = metric,
    scale = scale,
    ...
  )
  umap <- tibble::tibble(
    x = umap[,1],
    y = umap[,2],
  )

  clusters <- tibble(
    from = character(0),
    to = character(0),
    n = integer(0),
    ids = list()
  )

  res <- list(
    umap = umap,
    data = d,
    clusters = clusters,
    data_sup = data_sup,
    call = match.call(),
    seed = seed
  )

  class(res) <- c(class(res), "umapscan")

  res
}



#' @param us umapscan object to be printed
#' @param ... arguments passed to other methods
#'
#' @export

print.umapscan <- function(us, ...) {

  cat("\nCall: ", paste(deparse(us$call), sep = "\n", collapse = "\n"),
    "\n\n", sep = "")
  dims <- dim(us$data)
  cat("UMAP embeddings of a ", dims[1], "x", dims[2], " data frame\n", sep = "")

  if (!is.null(us$data_sup)) {
    dims_sup <- dim(us$data_sup)
    cat ("with a ", dims_sup[1], "x", dims_sup[2],
      " data frame of supplementary data\n", sep = "")
  }

  cat("\nClusters :")
  if (nrow(us$cluster) > 0) {
    cat("\n\n")
    print(data.tree::as.Node(us$clusters, mode = "network"))
  } else {
    cat(" <none>\n\n")
  }

  invisible(us)
}




#' @param us umapscan object to be plotted
#' @param color variable to highlight points (`tidylang` style). Must be a variable
#'   of the `data` or `data_sup` elements of `us`
#' @param palette `viridis` (for continuous `color`) or `brewer` (for categorical
#'   `color`) palette name
#' @param label legend label
#' @param ellipses if TRUE and `color` is categorical, add repartition ellipses
#' @param alpha points transparency
#' @param fixed if TRUE, add [ggplot2::coord_fixed()] to plot
#' @param ... arguments passed to other methods
#'
#' @examples
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1)
#' plot(us, color = Species, ellipses = TRUE)
#' plot(us, color = Sepal.Width, palette = "viridis", alpha = 0.5, fixed = TRUE)
#'
#' @export
#' @import ggplot2

plot.umapscan <- function(
  us, color, palette, label = NULL, ellipses = FALSE, alpha = 0.2, fixed = FALSE
) {

  if (!inherits(us, "umapscan")) stop("`us` must be an object of class umapscan.")

  d <- dplyr::bind_cols(us$umap, us$data, us$data_sup)

  g <- ggplot(d, aes(x = x, y = y)) +
    xlab("") +
    ylab("")

  if (fixed) {
    g <- g + coord_fixed()
  }

  if (missing(color)) {
    g <- g + geom_point(alpha = alpha)
    return(g)
  }

  g <- g + geom_point(aes(color = {{color}}), alpha = alpha)

  col_var <- d %>% dplyr::pull({{color}})

  if (is.numeric(col_var)) {
    if (missing(palette)) palette <- "inferno"
    g <- g + scale_color_viridis_c(label, option = palette)
  } else {
    if (missing(palette)) {
      scale <- qualitative_palette(col_var, label)
    } else {
      scale <- scale_color_brewer(label, palette = palette)
    }
    g <- g + scale
  }

  if (ellipses) {
    g <- g + stat_ellipse(aes(color = {{color}}))
  }

  g

}

