test_that("Viz selection", {
  data <- iris
  dic <- homodatum::create_dic(data)
  names(data) <- dic$id
  data_viz <- variable_selection(data = data, path = NULL, viz = NULL, "species")
  data_viz  <- var_aggregation(data_viz,
                               dic, agg = "count",
                               group_var = "species",
                               to_agg = "species",
                               name = "Total")
  data_result <- viz_selection(data = data_viz,
                               dic = dic,
                               viz = "map", num_hType = TRUE)
  expect_equal(data_result, "lfltmagic::lflt_choropleth_GnmNum")

  data_viz <- variable_selection(data = data,
                                 path = NULL,
                                 viz = "bar",
                                 "species", "sepal_width")
  data_viz  <- var_aggregation(data_viz,
                               dic, agg = "mean",
                               group_var = "species",
                               to_agg = "sepal_width",
                               name = "Total")
  data_result <- viz_selection(data = data_viz,
                               dic = dic,
                               viz = "bar", num_hType = TRUE)
  expect_equal(data_result, "hgchmagic::hgch_bar_CatNum")


})
