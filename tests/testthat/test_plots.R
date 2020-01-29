context("umapscan plots")

skip_on_cran()
skip_on_travis()
skip_on_ci()

iris_num <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris_sup <- iris[, "Species", drop=FALSE]
us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 1337, scale = FALSE)

us_plot <- plot(us)
us_plot_color <- plot(us, color = Species)
us_plot_full <- plot(us, color = Species, label = "test", ellipses = TRUE, alpha = 0.8, fixed = TRUE)

vdiffr::expect_doppelganger("Base umapscan plot", us_plot)
vdiffr::expect_doppelganger("Colored umapscan plot", us_plot_color)
vdiffr::expect_doppelganger("Full umapscan plot", us_plot_full)
