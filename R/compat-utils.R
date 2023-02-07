
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
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   paste_vector, .names = by))
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
      dplyr::summarise(...count = dplyr::n())
    if (!is.null(name)) {
      names(dd)[ncol(dd)] <- name
    }
  } else {
    dd <- data |>
      dplyr::group_by(dplyr::across(group_var)) |>
      dplyr::summarise(dplyr::across(to_agg,
                                     ~dsapptools:::aggregation(agg, .x),
                                     .names = name))
  }
  dd
}


make_buttons <- function(ids = NULL, labels = NULL,
                         default_active = NULL,
                         class_buttons = "",
                         class = "needed",
                         class_active = "basic_active") {
  if (is.null(ids)) return()
  if (is.null(labels)) return()

  df <- data.frame(id = ids, questions = labels)
  l <- purrr::map(1:nrow(df), function(z){
    div(class = class_buttons,
    shiny::actionButton(inputId = df[z,]$id,
                        label = htmltools::HTML(df[z,]$questions),
                        class = class)
    )
  })

  if (!is.null(default_active)) {
  l[[default_active]] <- gsub(class, paste(class, class_active), l[[default_active]])
  l[[default_active]] <- htmltools::HTML(paste0(paste(l[[default_active]], collapse = '')))
  }

  l
}



