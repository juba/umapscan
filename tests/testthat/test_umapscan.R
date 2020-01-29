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
  result <- structure(list(x = c(11.6932533257899, 8.48682209032854, 8.94537694989543,
    8.63233807308002, 11.3944929366678, 11.1320853533792, 10.996683944592,
    11.4701933339384, 8.32320755755278, 8.712340491116, 11.347654991807,
    11.2364195941491, 8.43106511748503, 8.48006552493561, 11.011713721317,
    11.2008313597823, 11.1435136618747, 11.5583933959064, 11.0725434932137,
    11.3452551289649, 11.7709458185831, 11.284237380704, 11.1309463605399,
    11.5597234405371, 11.2181220110811, 8.55635273512435, 11.3492282252913,
    11.705346618466, 11.7551334270898, 8.9667453666584, 8.69178678622884,
    11.7427585181045, 11.0610423411481, 10.9764977281044, 8.73274647587698,
    8.8379191624165, 11.7708651922152, 11.2581813018654, 8.51582841429607,
    11.5395932721063, 11.4110236602746, 8.22357325656342, 8.86842654603632,
    11.4530219436469, 11.2236240774519, 8.48862722552256, 11.2302941860474,
    8.92397773900163, 11.2518222717529, 11.4415306580137), y = c(4.32582184307899,
      6.89088896922506, 6.3222512863545, 6.36276616939989, 4.07788702298521,
      3.40000329269828, 5.17873596897967, 5.02346424586375, 6.39071713619119,
      6.94687931185683, 3.72150891466455, 4.96904854537037, 6.76700449747471,
      6.40090640989239, 3.19132465178415, 3.10156111228133, 3.41217265182062,
      4.47297461447579, 3.32718390419867, 3.60708617596377, 4.81358543955574,
      3.6307173896646, 4.46001588484803, 5.00216882231412, 4.96899905715005,
      6.83991149312032, 4.75611563994172, 4.47966466512327, 4.90590131621251,
      6.49350431387249, 6.73955817367622, 4.76323728261668, 3.26854453512417,
      3.15798482663156, 6.79814021915006, 6.86402626696063, 4.69266473027975,
      4.21096398244915, 6.43688085294924, 5.02385925551263, 4.42143499128278,
      6.55537196673917, 6.43362822779334, 4.58597937033648, 3.53632692730365,
      6.74697742762796, 3.54316410518693, 6.51514238248617, 3.56464170478963,
      5.2160293513385)), class = "data.frame", row.names = c(NA, -50L
      ))
  expect_equal(us$umap %>% dplyr::slice(1:50) %>% data.frame, result)
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




