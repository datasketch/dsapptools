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
