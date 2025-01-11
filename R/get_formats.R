#' @title Determine File Format
#'
#' @description
#' Determine file format from its extension.
#'
#' @param extensions Vector with file extensions.
#'
#' @return Vector with all possible file extensions for the known formats. File extensions wich are not recognized (and for which a format cannot be determined) are signed with an "Unknown" tag.
#' @export
#'
#' @examples
#' get_format(c("htm", "txt","random_tag"))
#' get_format("mpg")

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
