test_that("Var agg", {
  library(homodatum)

  data <- homodatum::sample_data("Cat-Cat-Num-Num-Cat")
  names(data) <- c("a", "b", "c", "d", "e")
  dic <- homodatum::create_dic(data)
  names(data) <- dic$id
  data_result <- var_aggregation(data = data,
                                 dic = dic,
                                 agg = "count",
                                 to_agg = "c",
                                 name = "Conteo",
                                 group_var = c("a", "b"))

  data_expect <- data |>
    dplyr::group_by(a, b) |>
    dplyr::summarise(Conteo = dplyr::n())

  expect_equal(data_result, data_expect)

  data_result <- var_aggregation(data = data,
                                 dic = dic,
                                 agg = "sum",
                                 to_agg = "c",
                                 name = "Suma",
                                 group_var = c("a", "b"))

  data_expect <- data |>
    dplyr::group_by(a, b) |>
    dplyr::summarise(Suma = sum(c, na.rm = TRUE))

  expect_equal(data_result, data_expect)

  data_result <- var_aggregation(data = data,
                                 dic = dic,
                                 agg = "mean",
                                 to_agg = "c",
                                 name = "Promedio",
                                 group_var = "e")

  data_expect <- data |>
    dplyr::group_by(e) |>
    dplyr::summarise(Promedio = mean(c, na.rm = TRUE))

  expect_equal(data_result, data_expect)
})
