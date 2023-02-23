#' @title Keep rows that match a condition
#'
#' @description The `data_filter()` function is used to subset a data frame, retaining all rows that satisfy a given condition. This function helps reducing code writing when building a shiny app.
#'
#' @param data A data frame
#' @param dic A data frame dictionary. This can be created by `create_dic()` function from `homodatum`package. Creating a dictionary allow `data_filter()` to detect different types of values (Dat, Cat, Num or list). This types of values belong to homodatum schema and can be checked by `homodatum::available_hd_Types()` function.
#' @param var_inputs A list where columns columns and condition must be specified.
#' @param special_placeholder Default as NULL. A character ("Todos" / "All") that adds a clickable filtering option when building a shiny app.
#' @param .id a character that specifies a the data frame variable id.
#'
#' @examples
#'
#' data <- iris
#' dic <- homodatum::create_dic(data)
#' names(data) <- dic$id
#' var_inputs <- list("species" = c("All"))
#' data_result <- data_filter(data,
#'                            dic,
#'                            var_inputs = var_inputs,
#'                            special_placeholder = "All")
#'
#'  # checking results
#'  unique(data_result$species) == unique(data$species)
#'
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

  purrr::reduce(var_inputs, .init = data, .f = function(df, var) {
    if (!is.null(var) && !setequal(var, "") && !(setequal(var, special_placeholder) && !is.null(special_placeholder))) {
      name_var <- names(var_inputs)
      info_var <- dic %>% dplyr::filter(id %in% name_var)
      filter_var <- var
      if (info_var$hdType %in% c("Dat", "Num", "numeric", "integer")) {
        df <- filter_ranges(df, range = filter_var, by = name_var)
      } else if (info_var$hdType == "list") {
        df <- filter_list(df, filter_var, name_var, .id = .id)
      } else if (info_var$hdType == c("Cat", "character", "factor")) {
        df <- df  |>  dplyr::filter(!!dplyr::sym(name_var) %in% filter_var)
      } else {
        df <- df
      }
    } else {
      df <- df
    }
    df
  })

  #return(df)
}

