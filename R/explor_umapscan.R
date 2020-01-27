#' Launch a shiny interface for umapscan object and clustering exploration
#'
#' @param us an umapscan object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.7)
#' us <- compute_clusters(us, minPts = 3, eps = 0.45, parent = "3")
#' us <- compute_clusters(us, minPts = 3, eps = 0.3, parent = "1")
#' explor_umapscan(us)
#' }
#'
#' @import shiny
#' @importFrom rlang sym
#' @importFrom graphics plot
#' @importFrom collapsibleTree renderCollapsibleTree collapsibleTreeOutput

explor_umapscan <- function(us) {

  vars_data <- names(us$data)
  vars_data_sup <- names(us$data_sup)

  shiny::shinyApp(
    ui = shiny::navbarPage(
      title = "umapscan",

      tabPanel(
        "Data",
        sidebarPanel(
          selectInput(
            "var",
            "Variable",
            choices = list(
              `data variables` = as.list(vars_data),
              `data_sup variables` = as.list(vars_data_sup)
            )
          ),
          sliderInput(
            "alpha_data",
            "Point opacity",
            min = 0.1,
            max = 1,
            step = 0.1,
            value = 0.5
          ),
          checkboxInput(
            "ellipses_data",
            "Ellipses",
            value = FALSE
          ),
          checkboxInput(
            "fixed_data",
            "Fixed coordinates",
            value = FALSE
          ),
        ),
        mainPanel(
          plotOutput("umap_plot")
        )
      ),

      tabPanel(
        "Clusters",
        div(class= "col-md-4",
          wellPanel(
             sliderInput(
              "alpha_clusters",
              "Point opacity",
              min = 0.1,
              max = 1,
              step = 0.1,
              value = 0.5
            ),
            checkboxInput(
              "ellipses_clusters",
              "Ellipses",
              value = FALSE
            ),
            checkboxInput(
              "fixed_clusters",
              "Fixed coordinates",
              value = FALSE
            ),
            checkboxInput(
              "noise_parent_clusters",
              "<Noise> inherits parent",
              value = FALSE
            ),
          )
        ),
        div(class= "col-md-8",
          collapsibleTree::collapsibleTreeOutput("umap_tree"),
          plotOutput("umap_clusters")
        )
      )

    ),

    server = function(input, output, session) {

      collapsed_nodes <- character()

      collapsed <- reactive({
        if (is.null(input$node)) return(collapsed_nodes)
        if (length(input$node) == 0) return(collapsed_nodes)
        input$clicked
        value <- input$node[[length(input$node)]]
        present <- value %in% collapsed_nodes
        if (present) {
          collapsed_nodes <<- collapsed_nodes[collapsed_nodes != value]
        } else {
          collapsed_nodes <<- c(collapsed_nodes, value)
        }
        collapsed_nodes
      })

      tree <- reactive({
        if(length(collapsed()) == 0) return(us)
        collapse_clusters(us, collapsed())
      })

      output$umap_plot <- renderPlot({
        var <- rlang::sym(input$var)
        graphics::plot(us, color = !!var,
          alpha = input$alpha_data,
          ellipses = input$ellipses_data,
          fixed = input$fixed_data)
      })

      output$umap_tree <- collapsibleTree::renderCollapsibleTree({
        umapscan_tree(us, max_label_length = 20)
      })

      output$umap_clusters <- renderPlot({
        plot_clusters(tree(),
          alpha = input$alpha_clusters,
          ellipses = input$ellipses_clusters,
          fixed = input$fixed_clusters,
          noise_inherit_parent = input$noise_parent_clusters
        )
      })

    }
  )
}


#' Convert a clusters element of an umapscan object to a collapsibleTree
#'
#' @param us an umapscan object
#' @param max_label_length maximum length of tree node labels
#'
#' @importFrom data.tree as.Node
#' @importFrom collapsibleTree collapsibleTree

umapscan_tree <- function(us, max_label_length = 100) {

  tree <- data.tree::as.Node(us$clusters, mode = "network")

  tree$Do(function(node) {
    if (nchar(node$name) > max_label_length) {
      node$name <- substr(node$name, 1, max_label_length)
      node$name <- paste0(node$name, "...")
    }
  })

  collapsibleTree::collapsibleTree(
    tree,
    collapse = FALSE,
    tooltip = TRUE,
    attribute = "n",
    inputId = "node",
    inputClickedId = "clicked",
  )

}

