context("umapscan class function")

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
  expect_equal(us$cluster, rep(NA_character_, nrow(iris_num)))
  expect_equal(us$cluster_validated, rep(FALSE, nrow(iris_num)))
  expect_equal(dim(us$umap), c(nrow(iris_num), 2))
  set.seed(1337)
  umap <- uwot::umap(iris_num)
  expect_equal(tibble::tibble(x = umap[,1], y = umap[,2]), us$umap)
})

test_that("new_umapscan results with scale=TRUE are ok", {
  us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 24312, scale = TRUE)
  set.seed(24312)
  umap <- uwot::umap(mutate_all(iris_num, base::scale))
  expect_equal(tibble::tibble(x = umap[,1], y = umap[,2]), us$umap)
})




