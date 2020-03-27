
iris_num <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris_sup <- iris[, "Species", drop = FALSE]
us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 1337, scale = TRUE)
us <- clust_compute(us, minPts = 3, eps = 1, graph = FALSE)
clust_describe(us)

us2 <- clust_compute(us, minPts = 3, eps = 0.3, alpha = 1, parent = "2")
us3 <- clust_compute(us2, minPts = 3, eps = 0.2, alpha = 1, parent = "2_1")
us3

us4 <- clust_compute(us3, minPts = 3, eps = 0.3, alpha = 1, parent = "1")
us4


us4$clusters
us4 <- clust_label(us4, "3", "Foobar")
clust_members(us4)
clust_members(us4, labels = FALSE)

## categorical

gen_var <- function() { sample(c(0,1), 100, replace = TRUE) }
set.seed(13)
df <- data.frame(v1 = gen_var(), v2 = gen_var(), v3 = gen_var())
us <- new_umapscan(df, n_neighbors = 25, min_dist = 0.1, seed = 1111, metric = "cosine")
us <- clust_compute(us, minPts = 3, eps = 0.4)
clust_describe(us, type = "barplot")

## textual

library(quanteda)
corpus <- data_corpus_inaugural
dtm <- dfm(corpus, remove_punct = TRUE)
dtm <- dfm_trim(dtm, min_termfreq = 50)

us <- new_umapscan(dtm, n_neighbors = 5, min_dist = 0.1, metric = "cosine")
us <- clust_compute(us, minPts = 3, eps = 1)
clust_describe(us, type = "keyness")
us2 <- clust_compute(us, minPts = 3, eps = .3, parent = "2")
clust_describe(us2, type = "keyness")
clust_describe(us2, type = "keyness", n_terms = 10, free_scale = FALSE, text_size = 15)
clust_describe(us2, type = "keyness", keyness_measure = "lr")
clust_describe(us2, type = "keyness", keyness_measure = "exact")
clust_describe(us2, type = "keyness", keyness_measure = "pmi")
