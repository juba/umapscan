
devtools::load_all(".")

iris_num <- iris %>% select_if(is.numeric)
iris_sup <- iris %>% select(Species)
us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1, seed = 1111)
us
us <- compute_clusters(us, minPts = 20, eps = 1, alpha = 1, graph = TRUE)
describe_clusters(us)

us2 <- compute_clusters(us, minPts = 3, eps = 0.5, alpha = 1, parent = "3")
us3 <- compute_clusters(us2, minPts = 3, eps = 0.2, alpha = 1, parent = "3_2")
us3

rename_cluster(us3, "3_1_1", "foo")

us3
