#' Database of known file extensions
#'
#' Contains file extensions grouped by file format, a short description of the format and a format category.
#'
#' @format A data frame with 1717 rows and 3 columns.
#' \describe{
#'     \item{Extension}{List of known file extensions grouped by format.}
#'     \item{Description}{Short description of the file extensions.}
#'     \item{Category}{File format category.}
#'     }
#'
#' @source {Adapted from https://en.wikipedia.org/wiki/List_of_file_formats}
#'
#' @examples
#' data(known_db) #Lazy loading.
"known_db"
