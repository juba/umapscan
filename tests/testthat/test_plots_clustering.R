context("umapscan clustering plots")

skip_on_cran()
skip_on_travis()
skip_on_ci()

iris_num <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris_sup <- iris[, "Species", drop=FALSE]
us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 1337, scale = TRUE)
us <- clust_compute(us, minPts = 3, eps = 1, graph = FALSE)
us <- clust_compute(us, minPts = 3, parent = "3", eps = 0.4, graph = FALSE)
us <- clust_label(us, "2", "Cluster 2")
us <- clust_label(us, "3_3", "Cluster 3_3")

cluster_plot <- clust_plot(us)
cluster_plot_noise <- clust_plot(us, noise_inherit_parent = TRUE)
cluster_plot_full <- clust_plot(us, alpha = 0.5, ellipses = FALSE, fixed = TRUE, labels = FALSE)
cluster_describe_boxplot <- clust_describe(us)
cluster_describe_labels <- clust_describe(us, labels = FALSE)
cluster_describe_ridges <- clust_describe(us, type = "ridges")


## barplot

gen_var <- function() { sample(c(0,1), 100, replace = TRUE) }
set.seed(13)
df <- data.frame(v1 = gen_var(), v2 = gen_var(), v3 = gen_var())
us_cat <- new_umapscan(df, n_neighbors = 25, min_dist = 0.1, seed = 1111, metric = "cosine")
us_cat <- clust_compute(us_cat, minPts = 3, eps = 0.4)
cluster_describe_barplot <- clust_describe(us_cat, type = "barplot")

## keyness

library(quanteda)
corpus <- data_corpus_inaugural
dtm <- dfm(corpus, remove_punct = TRUE)
dtm <- dfm_trim(dtm, min_termfreq = 50)
us_text <- new_umapscan(dtm, n_neighbors = 5, min_dist = 0.1, metric = "cosine", seed = 1111)
us_text <- clust_compute(us_text, minPts = 3, eps = 0.7)
cluster_describe_keyness <- clust_describe(us_text, type = "keyness")
cluster_describe_keyness_full <- clust_describe(us_text, type = "keyness", n_terms = 10, text_size = 12, free_scale = FALSE)
cluster_describe_keyness_lr <- clust_describe(us_text, type = "keyness", keyness_measure = "lr")


## Tests

vdiffr::expect_doppelganger("Base cluster plot", cluster_plot)
vdiffr::expect_doppelganger("Noise cluster plot", cluster_plot_noise)
vdiffr::expect_doppelganger("Full cluster plot ", cluster_plot_full)
vdiffr::expect_doppelganger("Boxplot clusters description", cluster_describe_boxplot)
vdiffr::expect_doppelganger("Boxplot clusters description without labels", cluster_describe_labels)
vdiffr::expect_doppelganger("Ridges clusters description", cluster_describe_ridges)
vdiffr::expect_doppelganger("Barplot clusters description", cluster_describe_barplot)
vdiffr::expect_doppelganger("Keyness clusters description", cluster_describe_keyness)
vdiffr::expect_doppelganger("Keyness full clusters description", cluster_describe_keyness_full)
vdiffr::expect_doppelganger("Keyness lr clusters description", cluster_describe_keyness_lr)


