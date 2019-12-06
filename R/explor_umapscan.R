#' @export
#' @import shiny

explor_umapscan <- function(us) {

  vars_data <- names(us$data)
  vars_data_sup <- names(us$data_sup)

  shiny::shinyApp(
    ui = shiny::navbarPage(
      title = "umapscan",

      tabPanel(
        "UMAP plot",
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
            "alpha",
            "Point opacity",
            min = 0,
            max = 1,
            step = 0.1,
            value = 0.5
          ),
          checkboxInput(
            "ellipses",
            "Ellipses",
            value = FALSE
          ),
          checkboxInput(
            "fixed",
            "Fixed coordinates",
            value = FALSE
          ),

        ),
        mainPanel(
          plotOutput("umap_plot")
        )
      ),

      tabPanel(
        "Clusters description",
        NULL
      )

    ),

    server = function(input, output, session) {

      output$umap_plot <- renderPlot({
        var <- rlang::sym(input$var)
        plot(us, color = !!var,
          alpha = input$alpha,
          ellipses = input$ellipses,
          fixed = input$fixed)
      })

    }
  )
}
