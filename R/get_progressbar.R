#' @title Progress Bar
#'
#' @description
#' Initialize progress bar.
#'
#' @param total Total number of computer iterations for completion.
#'
#' @examples
#' get_progressbar(50)

get_progressbar = function(total){

  pb = progress::progress_bar$new(
    format="  Processing [:bar] :percent eta: :eta",
    total=total, clear=FALSE, width=60
  )

}
