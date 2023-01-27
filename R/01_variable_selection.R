#' @title Subset columns by their names
#'
#' @description The `data_select()` function is used to subset an specific columns from a data frame by their names or by defining them in a YAML configuration file. This function helps reducing code writing when building a shiny app.
#'
#' @param data A data frame
#' @param path path to the YAML to be called if necessary. Default value is NULL.
#' @param viz Specific type of viz when selecting by a YAML configuration file. Default value is NULL.
#' @param ... One or more quoted expressions (variable names) separated by commas.
#'
#' @examples
#'
#' data <- iris
#'
#' data_result <- variable_selection(data = iris,
#'                                   viz = NULL,
#'                                   path = NULL,
#'                                   "Sepal.Length", "Petal.Length")
#'
#'  names(data_result)
#'
#' @export
variable_selection <- function(data, path = NULL, viz = NULL, ...) {
  if (is.null(data)) return()

  if (!is.null(path)) {
    path <- paste0(path, ".yaml")
    conf_viz <- yaml::read_yaml(path)
    selected <- conf_viz[[viz]]
    if (length(selected) == 1) {
      data <- data |> dplyr::select({{ selected }})
    } else {
      data <- data[, selected]
    }
  } else {
    data <- data |> dplyr::select(...)
  }
  data
}
