#' Returns a dataframe with the coordinates of a unique body diagram
#'
#' @description
#' Outputs a dataframe of selected body diagram. Use to render the desired body diagram.
#'
#' @param data A dataframe. Input data from athlete. Default produces a generic body diagram.
#' @param model A character string. One of the available models. Call model_types(data = data) for full list. Default is 'futuristic_male
#'
#'@returns A data frame:
#'  \describe{
#'         \item{Id}{Numeric. Unique id of each muscle area.}
#'         \item{View}{A character String. The type of view (Anterior, Posterior, Left or Right)}
#'         \item{Part}{A character string. Upper or Lower Body.}
#'         \item{Group}{A character String. The name of the muscle group.}
#'         \item{Muscle}{A character String. The name of the muscle}
#'         \item{Side}{A character String. Whether it it left or right side of the body}
#'         \item{x}{A number. x coordinates}
#'         \item{y}{A number. y coordinates}
#'     }
#'
#' @export
#' @examples
#' bodygram(data = data)
#'

bodygram <- function(data = models, model = "futuristic_male"){

  models %>%
    dplyr::filter(Model == model) %>%
    dplyr::select(-Model)

}
