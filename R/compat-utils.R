
filter_ranges <- function(data, range, by) {
  if (is.null(data)) return()

  min_date <- min(data[[by]], na.rm = TRUE)
  max_date <- max(data[[by]], na.rm = TRUE)

  if (length(range) == 2) {
    if (min_date == range[1] & max_date == range[2]) {
      data_filter <- data
    } else {
      data_filter <- data |>
        dplyr::filter(!!dplyr::sym(by) >= range[1] &
                        !!dplyr::sym(by) <= range[2])
    }
  } else {
    data_filter <-  data |>
      dplyr::filter(!!dplyr::sym(by) == range)
  }
  data_filter

}

paste_vector <- function(x, collapse = ",") {
  paste0(trimws(unique(x)), collapse = collapse)
}

# filter variables with several categories in one row
filter_list <- function(data, cats, by, .id) {
  if (is.null(data)) return()
  if (is.null(cats)) return()
  if (is.null(by)) return()

  temporal_df <- data[,c(.id, by)] |>
    tidyr::separate_rows({{ by }}, sep = ",") |>
    dplyr::filter(!!dplyr::sym(by) %in% cats) |>
    dplyr::group_by(!!dplyr::sym(.id)) |>
    dplyr::summarise_each(dplyr::funs(paste_vector))
  data <- data[, -grep(by, names(data))]
  data <- data |> dplyr::inner_join(temporal_df)
  data
}

aggregation <- function (aggregation, ...) {
  if (is.null(aggregation)) return()
  if (is.na(aggregation)) return()
  do.call(aggregation, list(..., na.rm = TRUE))
}

aggregation_data <- function (data, agg, group_var, to_agg, name = NULL) {

  if (agg == "count") {
    dd <- data |>
      dplyr::group_by(dplyr::across({{ group_var }})) |>
      dplyr::summarise(dplyr::across(to_agg, ~dplyr::n(), .names = name))
  } else {
    dd <- data |>
      dplyr::group_by(dplyr::across(group_var)) |>
      dplyr::summarise(dplyr::across(to_agg,
                                     ~dsapptools:::aggregation(agg, .x),
                                     .names = name))
  }
  dd
}

