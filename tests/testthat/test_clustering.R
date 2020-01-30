context("umapscan clustering functions")

iris_num <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris_sup <- iris[, "Species", drop = FALSE]
us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 1337, scale = TRUE)
us <- compute_clusters(us, minPts = 3, eps = 0.6, graph = FALSE)
us <- compute_clusters(us, minPts = 3, parent = "3", eps = 0.4, graph = FALSE)
us <- compute_clusters(us, minPts = 3, parent = "2", eps = 0.15, graph = FALSE)

test_that("get cluster memberships", {
  # saveRDS(get_clusters_membership(us), "tests/values/get_cluster_membership1.rds")
  expect_equal(
    get_clusters_membership(us),
    readRDS("../values/get_cluster_membership1.rds")
  )
  # saveRDS(get_clusters_membership(us, parent = "3"), "tests/values/get_cluster_membership2.rds")
  expect_equal(
    get_clusters_membership(us, parent = "3"),
    readRDS("../values/get_cluster_membership2.rds")
  )
  # saveRDS(get_clusters_membership(us, max_level = 1), "tests/values/get_cluster_membership3.rds")
  expect_equal(
    get_clusters_membership(us, max_level = 1),
    readRDS("../values/get_cluster_membership3.rds")
  )
  # saveRDS(get_clusters_membership(us, noise_inherit_parent = TRUE, parent = "2"), "tests/values/get_cluster_membership4.rds")
  expect_equal(
    get_clusters_membership(us, noise_inherit_parent = TRUE, parent = "2"),
    readRDS("../values/get_cluster_membership4.rds")
  )
  expect_error(
    get_clusters_membership(us, parent = "<Noise>"),
    "Can't get membership starting from a <Noise> node"
  )
  expect_equal(
    get_clusters_membership(us, noise_inherit_parent = TRUE, parent = "8"),
    rep(NA_character_, 150)
  )
})


test_that("get_leaves", {
  res <- structure(list(from = c("", "2", "2", "2", "2", "2", "3", "3",
    "3", "3", "3", "3", "3"), to = c("1", "2_1", "2_2", "2_4", "2_3",
      "<Noise>", "3_1", "3_2", "3_3", "3_4", "3_5", "<Noise>", "3_6"
    )), row.names = c(NA, -13L), class = c("tbl_df", "tbl", "data.frame"
    ))
  expect_equal(umapscan:::get_leaves(us$clusters), res)
  res <- structure(list(from = c("2", "2", "2", "2", "2"), to = c("2_1",
    "2_2", "2_4", "2_3", "<Noise>")), row.names = c(NA, -5L), class = c("tbl_df",
      "tbl", "data.frame"))
  expect_equal(umapscan:::get_leaves(us$clusters, node = "2"), res)
  res <- structure(list(from = "foo", to = "1"), row.names = c(NA, -1L
  ), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(umapscan:::get_leaves(us$clusters, node = "1", parent = "foo"), res)
  expect_error(
    umapscan:::get_leaves(us$clusters, node = "<Noise>"),
    "Can't get leaves from a <Noise> node."
  )
})

test_that("get_ids", {
  expect_error(umapscan:::get_ids(us))
  expect_equal(
    get_ids(us, "2"),
    c(2L, 3L, 4L, 9L, 10L, 13L, 14L, 26L, 30L, 31L, 35L, 36L, 39L,
      42L, 43L, 46L, 48L)
  )
  expect_equal(
    get_ids(us, "3_4"),
    c(62L, 64L, 72L, 74L, 79L, 92L, 98L)
  )
  expect_null(get_ids(us, "3_8"))
  expect_equal(get_ids(us, ""), 1:nrow(iris))
  expect_error(get_ids(us, "<Noise>"), "Can't get ids from a <Noise> node.")
})






