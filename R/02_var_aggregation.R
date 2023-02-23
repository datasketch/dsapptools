#' @title Compute Summary Statistics of Data
#'
#' @description The `var_aggregation()` function is used to compute custom summary statistics from a data frame. This function helps reducing code writing and optimization when building a shiny app.
#'
#' @param data A data frame.
#' @param dic A data frame dictionary. This can be created by `create_dic()` function from `homodatum`package. Creating a dictionary allow `data_filter()` to detect different types of values (Dat, Cat, Num or list). This types of values belong to homodatum schema and can be checked by `homodatum::available_hd_Types()` function.
#' @param agg Type of aggregation to be calculated. If it exists in base R, it can be calculated (sum, count, mean, sd, etc.).
#' @param group_var variable(s) to group by.
#' @param to_agg variable to be aggregated when more than one are available.
#' @param name The name of the new column in the output.
#'
#' @examples
#' library(homodatum)
#' data <- homodatum::sample_data("Cat-Cat-Num-Num-Cat")
#'
#' names(data) <- c("a", "b", "c", "d", "e")
#'
#' dic <- homodatum::create_dic(data)
#'
#' names(data) <- dic$id
#'
#' data_result <- var_aggregation(data = data,
#'                                dic = dic,
#'                                agg = "count",
#'                                to_agg = "c",
#'                                name = "Conteo",
#'                                group_var = c("a", "b"))
#'
#' @export
var_aggregation <- function(data, dic = NULL, agg, group_var, to_agg, name = NULL) {
  if (is.null(data)) return()

  if (!is.null(dic)) {
    dic_filter <- dic |>
      dplyr::filter(dic$id %in% names(data))

    if ("list" %in% dic_filter$hdType) {
      var <- dic_filter |>
        dplyr::filter(hdType %in% "list") |>
        dplyr::pull(dic$id)
      data <- data |>
        tidyr::separate_rows( {{ var }}, sep = ",")
    }
  }
  data <- aggregation_data(data = data,
                           agg = agg,
                           group_var = group_var,
                           to_agg = to_agg,
                           name = name)
  data
}
