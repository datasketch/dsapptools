#' @title Choose the proper visualization for you data
#'
#' @description The `var_aggregation()` function is used to compute custom summary statistics from a data frame. This function helps reducing code writing and optimization when building a shiny app.
#'
#' @param data A data frame to use for plot
#' @param dic A data frame dictionary. This can be created by `create_dic()` function from `homodatum`package. Creating a dictionary allow `data_filter()` to detect different types of values (Dat, Cat, Num or list). This types of values belong to homodatum schema and can be checked by `homodatum::available_hd_Types()` function.
#' @param viz Type of visualization needed. Options available: *map, map_bubbles, line, bar*.
#' @param num_hType helper to set if the column with the data to be plotted is a Num type, from the `available_hdTypes()`.
#'
#' @return type of visualization needed from the `hgchmagic` package
#'
#' @examples
#'
#' data <- iris
#' dic <- homodatum::create_dic(data)
#' names(data) <- dic$id
#' data_viz <- variable_selection(data = data, path = NULL, viz = NULL, "species")
#' data_viz  <- var_aggregation(data_viz,
#'                              dic, agg = "count",
#'                              group_var = "species",
#'                              to_agg = "species",
#'                              name = "Total")
#' viz_result <- viz_selection(data = data_viz,
#'                              dic = dic,
#'                              viz = "map", num_hType = TRUE)
#'
#' @export
viz_selection <- function(data, dic, viz, num_hType = FALSE) {
  if (is.null(data)) return()
  if (nrow(data) == 0) return()
  if (is.null(dic)) return()
  if (is.null(viz)) return()

  dic_viz <- dic |>
    dplyr::filter(id %in% names(data))

  library <- "hgchmagic"
  pseudonym <- "hgch"

  if (viz == "map") {
    dic_viz$hdType[grepl("Cat", dic_viz$hdType)] <- "Gnm"
    viz <- "choropleth"
    library <- "lfltmagic"
    pseudonym <- "lflt"
  } else if (viz == "map_bubbles") {
    dic_viz$hdType <- c("Gln", "Glt")
    viz <- "bubbles"
    library <- "lfltmagic"
    pseudonym <- "lflt"
  } else if (viz == "line") {
    dic_viz$hdType[grepl("Dat", dic_viz$hdType)] <- "Yea"
  }  else {
    viz <- viz
  }

  if (num_hType) {
    num_hType <- "Num"
  } else {
    num_hType <- NULL
  }


  viz_type <- paste0(library, "::",
                     pseudonym, "_",
                     viz, "_",
                     paste0(dic_viz$hdType, collapse = ""),
                     num_hType)
  viz_type
}
