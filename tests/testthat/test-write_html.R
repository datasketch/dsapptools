test_that("Summary click in html", {
  data <- iris
  dic <- homodatum::create_dic(data)
  names(data) <- dic$id
  html_result <- write_html(data = data,
                            dic = dic,
                            click = list("species" = "setosa"),
                            class_title = "click-title",
                            class_body = "click-text",
                            id = NULL,
                            text_result_null  = NULL,
                            "species")
  html_expect <- htmltools::HTML(
    '<div class="click-title"><q>setosa</q></div><div class="click-text"></div>')

  expect_equal(html_result, html_expect)

})
