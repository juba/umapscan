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
  umap <- uwot::umap(iris_num)
  expect_equal(tibble::tibble(x = umap[,1], y = umap[,2]), us$umap)
})

test_that("new_umapscan results with scale=TRUE are ok", {
  us <- new_umapscan(iris_num, data_sup = iris_sup, seed = 24312, scale = TRUE)
  set.seed(24312)
  umap <- uwot::umap(dplyr::mutate_all(iris_num, base::scale))
  expect_equal(tibble::tibble(x = umap[,1], y = umap[,2]), us$umap)
})

test_that("new_umapscan results are reproducible", {
  us <- new_umapscan(iris_num, seed = 24312, scale = TRUE)
  result_x <- c(-7.44889737588631, -7.38192322573992, -7.37254465065946, -7.35454671167503,
    -7.33629241704345, -7.33504163070497, -7.3202606703062, -7.27287596386241,
    -7.25693235908436, -7.21718125466601, -7.13048624695502, -7.05765045059968,
    -7.04790061736436, -7.04108235166542, -7.00279953579331, -7.00077744979406,
    -6.99959678178197, -6.99088278922375, -6.9499267296621, -6.94552564166791,
    -6.88263376661561, -6.88029749656653, -6.86930057279603, -6.83958803428778,
    -6.79074415385934, -6.70247681248358, -6.62814588409949, -6.43482691358745,
    -6.39393870844617, -6.32862699111762, -6.31374200983875, -6.2875652766412,
    -6.28713578807392, -6.27379350425399, -6.26634801885776, -6.22418630988105,
    -6.15603191354588, -6.13275173762668, -6.04861656639117, -5.93579957338784,
    -5.76299626544774, -5.75419013976396, -5.74467765624922, -5.71073606325636,
    -5.69668233928301, -5.68511193328979, -5.64927161491823, -5.60895573422862,
    -5.37090169673842, -5.36944698581941)
  result_y <- c(0.158031783893353, 3.10156111228133, 3.15798482663156, 3.19132465178415,
    3.26854453512417, 3.32718390419867, 3.40000329269828, 3.41217265182062,
    3.53632692730365, 3.54316410518693, 3.56464170478963, 3.60708617596377,
    3.6307173896646, 3.72150891466455, 4.07788702298521, 4.21096398244915,
    4.32582184307899, 4.42143499128278, 4.46001588484803, 4.47297461447579,
    4.47966466512327)
  expect_equal(sort(us$umap$x)[1:50], result_x)
  expect_equal(sort(us$umap$y)[100:120], result_y)
})




