context("umapscan clustering plots")

skip_on_cran()

iris_num <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris_sup <- iris[, "Species", drop=FALSE]
us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 1337, scale = FALSE)
us <- compute_clusters(us, minPts = 3, eps = 0.6, graph = FALSE)
us <- compute_clusters(us, minPts = 3, parent = "3", eps = 0.3, graph = FALSE)

cluster_plot <- plot_clusters(us)
cluster_plot_noise <- plot_clusters(us, noise_inherit_parent = TRUE)
cluster_plot_full <- plot_clusters(us, alpha = 0.5, ellipses = FALSE, fixed = TRUE)
cluster_describe_boxplot <- describe_clusters(us)
cluster_describe_ridges <- describe_clusters(us, type = "ridges")

vdiffr::expect_doppelganger("Base cluster plot", cluster_plot)
vdiffr::expect_doppelganger("Noise cluster plot", cluster_plot_noise)
vdiffr::expect_doppelganger("Full cluster plot ", cluster_plot_full)
vdiffr::expect_doppelganger("Boxplot clusters description", cluster_describe_boxplot)
vdiffr::expect_doppelganger("Ridges clusters description", cluster_describe_ridges)
