#' @export
var_aggregation <- function(data, dic = NULL, agg, group_var, to_agg, name = NULL) {
  if (is.null(data)) return()

  if (!is.null(dic)) {
    dic_filter <- dic |>
      dplyr::filter(id %in% names(data))

    if ("list" %in% dic_filter$hdType) {
      var <- dic_filter |>
        dplyr::filter(hdType %in% "list") |>
        dplyr::pull(id)
      data <- data |>
        tidyr::separate_rows( {{ var }}, sep = ",")
    }
  }
  data <- dsapptools:::aggregation_data(data = data,
                                        agg = agg,
                                        group_var = group_var,
                                        to_agg = to_agg,
                                        name = name)
  data
}
