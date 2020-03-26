
describe_clusters(us)

us2 <- compute_clusters(us, minPts = 3, eps = 0.6, alpha = 1, parent = "2")
us3 <- compute_clusters(us2, minPts = 3, eps = 0.3, alpha = 1, parent = "2_2")
us3

us4 <- compute_clusters(us3, minPts = 3, eps = 0.3, alpha = 1, parent = "1")
us4

## categorical

gen_var <- function() { sample(c(0,1), 100, replace = TRUE) }
set.seed(13)
df <- data.frame(v1 = gen_var(), v2 = gen_var(), v3 = gen_var())
us <- new_umapscan(df, n_neighbors = 25, min_dist = 0.1, seed = 1111, metric = "cosine")
us <- compute_clusters(us, minPts = 3, eps = 0.4)
describe_clusters(us, type = "barplot")

## textual

library(quanteda)
corpus <- data_corpus_inaugural
dtm <- dfm(corpus, remove_punct = TRUE)
dtm <- dfm_trim(dtm, min_termfreq = 50)

us <- new_umapscan(dtm, n_neighbors = 5, min_dist = 0.1, metric = "cosine")
us <- compute_clusters(us, minPts = 3, eps = 1)
describe_clusters(us, type = "keyness")
us2 <- compute_clusters(us, minPts = 3, eps = .3, parent = "2")
describe_clusters(us2, type = "keyness")
describe_clusters(us2, type = "keyness", n_terms = 10, free_scale = FALSE, text_size = 15)
describe_clusters(us2, type = "keyness", keyness_measure = "lr")
describe_clusters(us2, type = "keyness", keyness_measure = "exact")
describe_clusters(us2, type = "keyness", keyness_measure = "pmi")
