#' @export
data_filter <- function(data,
                        dic,
                        var_inputs,
                        special_placeholder = NULL,
                        .id = NULL) {
  if (is.null(data)) return()
  if (is.null(dic)) return()
  df <- data
  if (is.null(var_inputs)) return()
  if (!is.list(var_inputs)) return()
  tem_ls <-
    seq_along(var_inputs) |>
    purrr::map(function(.x) {
      if (!is.null(var_inputs[[.x]])) {
        if (!setequal(var_inputs[[.x]], "")) {
          other_condition <- FALSE
          if (!is.null(special_placeholder)) {
            other_condition <- setequal(var_inputs[[.x]], special_placeholder)
          }
          if (!other_condition) {
            name_var <- names(var_inputs)[.x]
            info_var <- dic |>
              dplyr::filter(id %in% name_var)
            filter_var <- var_inputs[[.x]]
            if (info_var$hdType == "Dat") {
              df <<- filter_ranges(df, range = filter_var, by = info_var$id)
            }
            if (info_var$hdType == "list") {
              df <<- filter_list(df, filter_var, info_var$id, .id = .id)
            }
            if (info_var$hdType == "Cat") {
              df <<- df |>
                dplyr::filter(!!dplyr::sym(info_var$id) %in% filter_var)
            }
            if (info_var$hdType == "Num") {
              df <<- filter_ranges(df, range = filter_var, by = info_var$id)
            }
          }
        }
      }
    })
  rm(tem_ls)
  df
}

