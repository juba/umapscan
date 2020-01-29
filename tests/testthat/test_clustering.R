context("umapscan clustering functions")

iris_num <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris_sup <- iris[, "Species", drop = FALSE]
us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 1337, scale = TRUE)
us <- compute_clusters(us, minPts = 3, eps = 0.6, graph = FALSE)
us <- compute_clusters(us, minPts = 3, parent = "3", eps = 0.4, graph = FALSE)
us <- compute_clusters(us, minPts = 3, parent = "2", eps = 0.15, graph = FALSE)

test_that("get cluster memberships", {
  res <- c("1", "2_1", "2_2", "2_4", "1", "1", "1", "1", "2_3", "2_4",
    "1", "1", "2_4", "2_3", "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "2_1", "1", "1", "1", "2_2", "2_4", "1", "1",
    "1", "2_4", "<Noise>", "1", "1", "2_3", "1", "1", "2_3", "2_2",
    "1", "1", "2_1", "1", "2_2", "1", "1", "3_1", "3_1", "3_1", "3_2",
    "3_1", "3_3", "3_1", "3_2", "3_1", "3_2", "3_2", "3_4", "3_2",
    "3_4", "3_3", "3_1", "3_3", "3_2", "3_2", "3_2", "3_5", "3_4",
    "3_5", "3_4", "3_1", "3_1", "3_1", "<Noise>", "3_4", "3_2", "3_2",
    "3_2", "3_2", "3_5", "3_3", "3_1", "3_1", "3_2", "3_3", "3_2",
    "3_2", "3_4", "3_2", "3_2", "3_2", "3_3", "3_3", "3_4", "3_2",
    "3_3", "3_6", "3_5", "3_6", "3_6", "3_6", "3_6", "3_2", "3_6",
    "3_5", "3_6", "3_6", "3_5", "3_6", "3_5", "3_5", "3_6", "3_6",
    "3_6", "3_6", "3_2", "3_6", "3_5", "3_6", "3_5", "3_6", "3_6",
    "3_5", "3_5", "3_6", "3_6", "3_6", "3_6", "3_6", "3_5", "3_5",
    "3_6", "3_6", "3_6", "3_5", "3_6", "3_6", "3_6", "3_5", "3_6",
    "3_6", "3_6", "3_5", "3_6", "3_6", "3_5")
  expect_equal(get_clusters_membership(us), res)
  res <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, "3_1", "3_1", "3_1", "3_2", "3_1", "3_3", "3_1",
    "3_2", "3_1", "3_2", "3_2", "3_4", "3_2", "3_4", "3_3", "3_1",
    "3_3", "3_2", "3_2", "3_2", "3_5", "3_4", "3_5", "3_4", "3_1",
    "3_1", "3_1", "<Noise>", "3_4", "3_2", "3_2", "3_2", "3_2", "3_5",
    "3_3", "3_1", "3_1", "3_2", "3_3", "3_2", "3_2", "3_4", "3_2",
    "3_2", "3_2", "3_3", "3_3", "3_4", "3_2", "3_3", "3_6", "3_5",
    "3_6", "3_6", "3_6", "3_6", "3_2", "3_6", "3_5", "3_6", "3_6",
    "3_5", "3_6", "3_5", "3_5", "3_6", "3_6", "3_6", "3_6", "3_2",
    "3_6", "3_5", "3_6", "3_5", "3_6", "3_6", "3_5", "3_5", "3_6",
    "3_6", "3_6", "3_6", "3_6", "3_5", "3_5", "3_6", "3_6", "3_6",
    "3_5", "3_6", "3_6", "3_6", "3_5", "3_6", "3_6", "3_6", "3_5",
    "3_6", "3_6", "3_5")
  expect_equal(get_clusters_membership(us, parent = "3"), res)
  res <- c("1", "2", "2", "2", "1", "1", "1", "1", "2", "2", "1", "1",
    "2", "2", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
    "2", "1", "1", "1", "2", "2", "1", "1", "1", "2", "2", "1", "1",
    "2", "1", "1", "2", "2", "1", "1", "2", "1", "2", "1", "1", "3",
    "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3",
    "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3",
    "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3",
    "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3",
    "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3",
    "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3",
    "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3",
    "3", "3", "3", "3", "3", "3", "3", "3")
  expect_equal(get_clusters_membership(us, max_level = 1), res)
  res <- c(NA, "2_1", "2_2", "2_4", NA, NA, NA, NA, "2_3", "2_4", NA,
    NA, "2_4", "2_3", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    "2_1", NA, NA, NA, "2_2", "2_4", NA, NA, NA, "2_4", "2", NA,
    NA, "2_3", NA, NA, "2_3", "2_2", NA, NA, "2_1", NA, "2_2", NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA)
  expect_equal(get_clusters_membership(us, noise_inherit_parent = TRUE, parent = "2"), res)
  expect_error(get_clusters_membership(us, parent = "<Noise>"), "Can't get membership starting from a <Noise> node")
  expect_equal(get_clusters_membership(us, noise_inherit_parent = TRUE, parent = "8"), rep(NA_character_, 150))
})


# test_that("get cluster leaves", {
#   get_leaves(us)
#   get_leaves(us, node = "2")
# })




