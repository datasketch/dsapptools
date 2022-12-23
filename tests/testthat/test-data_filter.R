test_that("Data filter", {
  data <- iris
  dic <- homodatum::create_dic(data)
  names(data) <- dic$id
  var_inputs <- list("species" = c("All"))
  data_result <- data_filter(data,
                             dic,
                             var_inputs = var_inputs,
                             special_placeholder = "All")
  expect_equal(data_result, data)

  var_inputs <- list("species" = c("Todos"))
  data_result <- data_filter(data,
                             dic,
                             var_inputs = var_inputs,
                             special_placeholder = "Todos")
  expect_equal(data_result, data)

  var_inputs <- list("species" = c("versicolor"))
  data_result <- data_filter(data,
                             dic,
                             var_inputs = var_inputs)
  expect_equal(as.character(unique(data_result$species)), "versicolor")

})
