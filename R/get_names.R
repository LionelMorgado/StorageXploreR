#' @title Get File Name
#'
#' @description
#' Get file name (file name without extension) from a full file name tag for a set of files. Assumes that the tag follows the format: <file_name>.<file_extension>.
#'
#' @param fullfns Vector with strings to be parsed.
#'
#' @return Vector with filenames without extension.
#' @export
#'
#' @examples
#' get_names(c("amazing_file.txt", "bigdata.Rdata"))

get_names = function(fullfns){
  #..print message to user interface..
  tot_f = length(fullfns)
  print(paste0("Parsing file names in ", tot_f, " files."))

  #..get file name for each file..
  names = unlist(lapply(fullfns, get_name))

  #..print message with resume of results to user interface..
  print("Finished.")

  return(names)
}
