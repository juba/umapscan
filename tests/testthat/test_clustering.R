context("umapscan clustering functions")

iris_num <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris_sup <- iris[, "Species", drop = FALSE]
us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 1337, scale = TRUE)
us <- compute_clusters(us, minPts = 3, eps = 1, graph = FALSE)
us <- compute_clusters(us, minPts = 3, parent = "3", eps = 0.4, graph = FALSE)
us <- compute_clusters(us, minPts = 3, parent = "1", eps = 0.25, graph = FALSE)

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
  res <- structure(list(from = c("1", "1", "1", "1", "", "3", "3", "3",
    "3"), to = c("1_1", "<Noise>", "1_2", "1_3", "2", "3_1", "3_2",
      "3_3", "<Noise>")), row.names = c(NA, -9L), class = c("tbl_df",
        "tbl", "data.frame"))
  expect_equal(umapscan:::get_leaves(us$clusters), res)
  res <- structure(list(from = c("3", "3", "3", "3"), to = c("3_1", "3_2",
    "3_3", "<Noise>")), row.names = c(NA, -4L), class = c("tbl_df",
      "tbl", "data.frame"))
  expect_equal(umapscan:::get_leaves(us$clusters, node = "3"), res)
  res <- structure(list(from = "foo", to = "2"), row.names = c(NA, -1L
  ), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(umapscan:::get_leaves(us$clusters, node = "2", parent = "foo"), res)
  expect_error(
    umapscan:::get_leaves(us$clusters, node = "<Noise>"),
    "Can't get leaves from a <Noise> node."
  )
})

test_that("get_ids", {
  expect_error(get_ids(us))
  expect_equal(
    get_ids(us, "2"),
    c(2L, 3L, 4L, 9L, 10L, 13L, 14L, 26L, 30L, 31L, 35L, 36L, 39L,
      42L, 43L, 46L, 48L)
  )
  expect_equal(
    get_ids(us, "3_2"),
    c(63L, 69L, 88L, 120L)
  )
  expect_null(get_ids(us, "3_8"))
  expect_equal(get_ids(us, ""), 1:nrow(iris))
  expect_error(get_ids(us, "<Noise>"), "Can't get ids from a <Noise> node.")
})

test_that("get_noise_ids", {
  expect_error(umapscan:::get_noise_ids(us))
  expect_equal(get_noise_ids(us, "3"), 134)
  expect_null(get_noise_ids(us, "2"))
  expect_error(get_noise_ids(us, "<Noise>"), "Can't get ids from a <Noise> node.")
})


test_that("get_cluster data", {
  expect_error(get_cluster_data(us))
  expect_equal(
    dim(get_cluster_data(us, "2")),
    c(17, 6)
  )
  expect_equal(
    get_cluster_data(us, "3_2")$umapscan_cluster,
    rep("3_2", 4)
  )
  expect_error(get_cluster_data(us, "<Noise>"), "Can't get data for a <Noise> cluster.")
})


test_that("rename_cluster", {
  # saveRDS(rename_cluster(us, "3_1", "foo")$clusters %>% tidyr::unnest(ids), "tests/values/rename1.rds")
  expect_equal(
    rename_cluster(us, "3_1", "foo")$clusters %>% tidyr::unnest(ids),
    readRDS("../values/rename1.rds")
  )
  expect_error(rename_cluster(us, "<Noise>", "foo"))
  out <- us %>% rename_cluster("3_2", "3_1")
  expect_equal(
    get_ids(out, "3_1"),
    c(get_ids(us, "3_1"), get_ids(us, "3_2"))
  )
  out <- us %>% rename_cluster("3_3", "<Noise>")
  expect_setequal(
    umapscan:::get_noise_ids(out, "3"),
    c(umapscan:::get_noise_ids(us, "3"), umapscan:::get_ids(us, "3_3"))
  )
  expect_error(
    rename_cluster(us, "3_2", "1_1"),
    "Can't rename a cluster with the same name as another cluster with another parent."
  )
})

test_that("remove_cluster", {
  expect_error(remove_cluster(us))
  expect_error(remove_cluster(us, "<Noise>"), "Can't remove a <Noise> cluster.")

  ## No test with rm_root = TRUE because the function should not
  ## be called directly with this argument.

  out <- umapscan:::remove_cluster(us, "3_2", rm_root = FALSE)
  expect_equal(
    out$clusters %>% tidyr::unnest(ids),
    us$clusters %>% tidyr::unnest(ids)
  )
  out <- umapscan:::remove_cluster(us, "2", rm_root = FALSE)
  expect_equal(
    out$clusters %>% tidyr::unnest(ids),
    us$clusters %>% tidyr::unnest(ids)
  )
  out <- umapscan:::remove_cluster(us, "3", rm_root = FALSE)
  expect_equal(
    dim(out$clusters %>% dplyr::filter(from == "3")),
    c(0, 5)
  )
})

test_that("remove_noise_cluster", {
  expect_error(remove_noise_cluster(us))
  expect_error(
    remove_noise_cluster(us, "<Noise>"),
    "Can't remove a <Noise> cluster from a <Noise> parent."
  )

  out <- umapscan:::remove_noise_cluster(us, "2")
  expect_equal(
    out$clusters %>% tidyr::unnest(ids),
    us$clusters %>% tidyr::unnest(ids)
  )

  out <- remove_noise_cluster(us, "1")
  res <- structure(list(from = c("1", "1", "1"), to = c("1_1", "1_2",
    "1_3")), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"
    ))
  expect_equal(get_leaves(out$clusters, "1"), res)

  out <- remove_noise_cluster(us, "3")
  expect_equal(sum(is.na(get_clusters_membership(out))), 1)

})


test_that("collapse_clusters", {

  out <- collapse_clusters(us, c("1", "3"))
  res <- structure(list(from = c("", "", ""), to = c("1", "2", "3")), row.names = c(NA,
    -3L), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(get_leaves(out$clusters), res)

})

