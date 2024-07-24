#' Body Diagram Model Types
#'
#' @description
#'
#' provides a list with the names of all the available diagram's models.
#'
#' @param data A dataframe. The dataframe with all the coordinates points.
#'
#' @return A data frame:
#'  \describe{
#'          \item{Model}{A factor. A column displaying all the available diagram model types}
#'     }
#'
#' @export
#' @examples
#' model_types()
#'
model_types <- function() {
  models %>%
    dplyr::pull(Model) %>%
    unique()
}
