#' Write HTML output
#'
#' This function generates HTML output based on a dataset and a dictionary, and applies click events to selected elements.
#'
#' @param data A dataset to display.
#' @param dic A dictionary used to format the output.
#' @param click A vector of column names to apply click events to.
#' @param class_title A CSS class to apply to the title.
#' @param class_body A CSS class to apply to the body.
#' @param text_result_null Text to display if the data is empty.
#' @param id An optional ID to apply to the HTML output.
#' @param ... Additional arguments to pass to \code{\link[dplyr]{select}}.
#'
#' @return An HTML output.
#'
#' @export
write_html <- function(data, dic, click, class_title,
                       class_body, text_result_null = NULL, id = NULL, ...) {
  if (is.null(data)) return()
  if (is.null(dic)) return()
  if (is.null(click)) return()
  data_click <- data_filter(data = data,
                            dic = dic,
                            var_inputs = click,
                            .id = id)

  if (nrow(data_click) == 0) {
    return(text_result_null)
  } else {

  data_click <- data_click |> dplyr::select(...)
  info_click <- names(data_click)

  htmltools::HTML(
    paste0(
      purrr::map(unique(data_click[[info_click[1]]]), function(x) {
        df <- data_click |> dplyr::filter(!!dplyr::sym(info_click[1]) %in% x)
        htmltools::HTML(
          paste0(

            htmltools::div(class = class_title,
                htmltools::HTML(
                  paste0(
                    "<q>", x, "</q>")
                )),
            htmltools::div(class = class_body,
                htmltools::HTML(
                  paste0(
                    purrr::map(names(df)[-1], function(v) {

                      tx <- paste0("<div class = 'click-p'><div class = 'click-tl'>", v, ":</div> <div class = 'click-subtitle'>",
                                   df[[v]], "</div></div>", collapse = "")
                      if (v == "url") {
                        tx <- paste0("<div style='display:flex;justify-content: space-between;'><div>",
                                     "<a href=", df[[v]]," target='_blank'>Link &#8734;</a>")
                      }
                      tx
                    }), collapse = "</br>" )
                )
            )
          )
        )
      }), collapse = "</br>" )
  )
  }
}

#' Write HTML output for grouped data
#'
#' This function creates an HTML output for a grouped data set.
#'
#' @param data A data frame to create the output from
#' @param dic A list of character vectors where each vector contains the names of the variables to be included in the output. The names of the list should be the group names
#' @param click A vector of variable names from data that will be used to filter the data. The HTML output is created only from the filtered data
#' @param text_result_null Text to be returned in case the filtered data is empty
#' @param separate_row Variable name that should be used to separate the rows in case it has multiple values
#' @param sep_url Separator used to separate multiple URLs in case they exist
#' @param url_name Name used to show the URL link in the HTML output
#' @param url_class CSS class for the URL link
#' @param id A variable name to use as an ID for the HTML output
#' @param ... Other arguments to be passed to \code{dplyr::select()}
#'
#' @return An HTML output for the filtered data
#'
#' @export
write_html_group <- function(data, dic, click,
                             text_result_null = NULL,
                             separate_row = NULL,
                             sep_url = ",",
                             url_name = "Link &#8734;",
                             url_class = "url-class",
                             id = NULL, ...) {

  if (is.null(data)) return()
  if (is.null(dic)) return()
  if (is.null(click)) return()
  data_click <- data_filter(data = data,
                            dic = dic,
                            var_inputs = click,
                            .id = id)

  if (nrow(data_click) == 0) {
    return(text_result_null)
  } else {

    data_click <- data_click |> dplyr::select(...)

   # htmltools::HTML(
    #  paste0(
   info <- purrr::map(1:nrow(data_click), function(r) {
      info <- data_click[r,]
      htmltools::div(class = "click-p",
     htmltools::HTML(
       paste0(
      purrr::map(names(info), function(v) {

       tx <- paste0("<div class = 'click-body'><div class = 'click-tl'>", v,
                    ":</div> <div class = 'click-info'>", info[[v]], "</div>
                    </div>", collapse = "")
       if (v == "url") {
         url_na <- FALSE
         url_na <- is.na(info[[v]])
        if (!is.null(separate_row)) {
         if (separate_row == "url") {
           info <- info |>
             tidyr::separate_rows(url, sep = sep_url) |>
             tidyr::drop_na(url)
          if (nrow(info) == 0) url_na <- TRUE
         }
        }
         if (!url_na) {
         tx <- paste0("<div class=", url_class, ">","<a href=", info[[v]]," target='_blank'>",
                      url_name,"</a></div>", collapse = "<br/>")
         }
       }
         tx
      }), collapse = ""))
      )
    })

   info


  }
}
