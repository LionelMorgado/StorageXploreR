#' @title Determine File Format
#'
#' @description
#' Determine file format from extension.
#'
#' @param extensions String vector with file extensions.
#'
#' @return String vector with all possible extensions for the detected format, for each file. File extensions wich are not recognized (and for which a format cannot be determined) are assigned the tag "Unknown".
#' @export
#'
#' @examples
#' get_formats(c("htm", "txt","random_tag"))
#' get_formats("mpg")

get_formats = function(extensions){

  #..print initialization message to user interface..
  tot_xt = length(extensions)
  print(paste0("Detecting formats for ", tot_xt, " extensions."))

  #..get extension for each file..
  format = get_format(extensions)

  #..print message with resume of results to user interface..
  known_tot = sum(format!="Unknown")
  known_perc = signif(known_tot/tot_xt*100, digits=3)
  print(paste0("File format detected for ", known_tot, " extensions (", known_perc ,"% of total)."))

  return(format)
}
