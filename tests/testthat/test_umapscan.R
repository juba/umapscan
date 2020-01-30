context("umapscan class functions")

iris_num <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris_sup <- iris[, "Species", drop=FALSE]

test_that("new_umapscan with wrong arguments throws error", {
  expect_error(new_umapscan(d = 1:10))
  expect_error(new_umapscan(d = iris_num, data_sup = 1:10))
  expect_error(new_umapscan(d = iris_num, data_sup = iris_sup[-1, ]))
})

test_that("new_umapscan results are ok", {
  us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 1337, scale = FALSE)
  expect_equal(us$data, iris_num)
  expect_equal(us$data_sup, iris_sup)
  expect_equal(dim(us$cluster), c(0, 5))
  expect_equal(dim(us$umap), c(nrow(iris_num), 2))
  set.seed(1337)
  umap <- uwot::umap(iris_num, n_sgd_threads = 0)
  expect_equal(tibble::tibble(x = umap[,1], y = umap[,2]), us$umap)
})

test_that("new_umapscan results with scale=TRUE are ok", {
  us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 24312, scale = TRUE)
  set.seed(24312)
  umap <- uwot::umap(dplyr::mutate_all(iris_num, base::scale),  n_sgd_threads = 0)
  expect_equal(tibble::tibble(x = umap[,1], y = umap[,2]), us$umap)
})

test_that("new_umapscan results are reproducible", {
  us <- new_umapscan(iris_num, seed = 24312, scale = TRUE)
  # saveRDS(us$umap %>% dplyr::slice(1:50) %>% data.frame, "tests/values/umapscan1.rds")
  expect_equal(
    us$umap %>% dplyr::slice(1:50) %>% data.frame,
    readRDS("../values/umapscan1.rds")
  )
})

test_that("set.seed is ok", {
  set.seed(556677)
  x <- runif(1)
  expect_equal(x, 0.05051134, tolerance = 0.00000001)
  y <- rnorm(1)
  expect_equal(y, 1.970592, tolerance = 0.000001)
  z <- runif(1)
  expect_equal(z, 0.3479608, tolerance = 0.0000001)
})

test_that("base umapscan with seed is ok", {
  set.seed(82223)
  umap <- uwot::umap(USArrests, n_sgd_threads = 0)
  # saveRDS(umap, "tests/values/umapscan2.rds")
  expect_equal(
    umap,
    readRDS("../values/umapscan2.rds")
  )
  set.seed(82223)
  umap_pca <- uwot::umap(USArrests, init = "pca", n_sgd_threads = 0)
  # saveRDS(umap_pca, "tests/values/umapscan3.rds")
  expect_equal(
    umap_pca,
    readRDS("../values/umapscan3.rds")
  )
})

test_that("uwot version is the same", {
  uwot_version <- installed.packages()["uwot",]
  res <- c(Package = "uwot",
    Version = "0.1.5", Priority = NA_character_, Depends = "Matrix", Imports = "Rcpp, methods, FNN, RSpectra, RcppAnnoy (>= 0.0.11),\nRcppParallel, irlba",
    LinkingTo = "Rcpp, RcppProgress, RcppParallel, RcppAnnoy, dqrng",
    Suggests = "testthat, covr", Enhances = NA_character_, License = "GPL-3",
    License_is_FOSS = NA_character_, License_restricts_use = NA_character_, OS_type = NA_character_,
    MD5sum = NA_character_, NeedsCompilation = "yes", Built = "3.6.2")
  expect_equal(uwot_version[names(uwot_version) != "LibPath"], res)
  })


