#' Generate cluster keyness statistics from a clustering result
#'
#' @param groups groups membership computed by `get_clusters_membership`
#' @param data object used to compute the clustering
#' @param keyness_measure statistics to compute
#' @param n_terms number of terms to display in keyness plots
#' @param show_negative if TRUE, show negative keyness features
#' @param max_p maximum keyness statistic p-value
#'
#' @seealso [quanteda::textstat_keyness()]
#'
#' @export
#'
#' @importFrom purrr map


keyness_stats <- function(
  groups, data,
  keyness_measure = c("chi2", "lr", "exact", "pmi"),
  n_terms = 15,
  show_negative = TRUE,
  max_p = 0.05) {

  if (!requireNamespace("quanteda", quietly = TRUE)) {
    stop("Package \"quanteda\" needed for this function to work. Please install it.",
      call. = FALSE)
  }

  keyness_measure <- match.arg(keyness_measure)

  stat_col <- switch(
    keyness_measure,
    "chi2" = "chi2",
    "lr" = "G2",
    "exact" = "or",
    "pmi" = "pmi"
  )
  stat_col <- rlang::sym(stat_col)

  dtm <- quanteda::as.dfm(data)

  groups_list <- sort(unique(groups))
  groups_list <- groups_list[!is.na(groups_list)]
  tabs <- purrr::map(groups_list, function(group) {
    select <- (groups == group & !is.na(groups))
    tab <- quanteda::textstat_keyness(dtm, select, measure = keyness_measure) %>%
      arrange(desc(abs(!!stat_col))) %>%
      filter(p < 0.05)
    if (show_negative) {
      tab %>%
        filter(p <= max_p) %>%
        slice(1:n_terms) %>%
        mutate(sign = if_else(!!stat_col > 0, "positive", "negative"),
          sign = factor(sign, levels = c("positive", "negative")))
    } else {
      tab %>%
        filter(!!stat_col > 0, p <= max_p) %>%
        slice(1:n_terms) %>%
        mutate(sign = "positive")
    }
  })
  names(tabs) <- groups_list

  tabs

}


## Generate a "terms bar plot", based on terms keyness for a group

keyness_barplot <- function(tab, range = NULL, title = "",
  stat_col = "chi2", n_terms = NULL, text_size = 10) {

  ## Column with statistic values
  stat_col_tidy <- rlang::sym(stat_col)
  if (!is.null(range)) {
    stat_max <- max(range)
  } else {
    stat_max <- max(tab[[stat_col]], na.rm = TRUE)
  }
  ## Plot
  g <- ggplot(data = tab, aes(x = stats::reorder(.data$feature, !!stat_col_tidy), y = abs(!!stat_col_tidy))) +
    geom_col(aes(fill = sign), color = "white", width = 1) +
    geom_text(y = stat_max / 15, aes(label = stats::reorder(.data$feature, !!stat_col_tidy)), hjust = 0, size = text_size / 2.5) +
    coord_flip() +
    scale_fill_manual("", guide = FALSE,
      values = c("positive" = "#a1d8ff", "negative" = "#ff7d7e")) +
    labs(title = title, x = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = text_size, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = text_size * 0.8),
      plot.margin = grid::unit(c(0.05,0.05,0,0), "npc"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = grDevices::rgb(.9,.9,.9,.2),
        colour = "transparent"))
  ## Fix x limits if necessary and remove horizontal axis values
  if (!is.null(range)) {
    g <- g + scale_y_continuous(stat_col, limits = range, breaks = NULL)
  } else {
    g <- g + scale_y_continuous(stat_col, breaks = NULL)
  }
  ## Adjust vertical scale if necessary
  if (nrow(tab) < n_terms) {
    limits <- levels(stats::reorder(tab$feature, tab[[stat_col]]))
    limits <- c(rep("", n_terms - length(limits)), limits)
    g <- g + scale_x_discrete(limits = limits, breaks = NULL)
  } else {
    g <- g + scale_x_discrete(breaks = NULL)
  }

  ## Align title element to the left to center it with hjust
  g <- ggplotGrob(g)
  g$layout$l[g$layout$name == "title"] <- 1
  g
}


