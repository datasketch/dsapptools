test_that("Var selection", {

  data_result <- variable_selection(data = iris,
                                    viz = NULL,
                                    path = NULL,
                                    "Sepal.Length", "Petal.Length")
  expect_equal(names(data_result), c("Sepal.Length", "Petal.Length"))

  data_result <- variable_selection(data = iris,
                                    viz = "bar",
                                    path = "tests/testthat/test_yaml/viz_conf")
  expect_equal(names(data_result), c("Species", "Petal.Width"))
})
