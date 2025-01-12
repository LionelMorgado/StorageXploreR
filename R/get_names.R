#' @title Get File Name
#'
#' @description
#' Get file name (name without extension) from a file full name tag for a set of files. Assumes that the tags follows the format: <name>.<extension>.
#'
#' @param files String vector containing files full name, concatenated or not with the path.
#'
#' @return String vector with file names without extension.
#' @export
#'
#' @examples
#' get_names(c("/mnt/amazin_project/amazing_file.txt", "bigdata.Rdata"))

get_names = function(files){
  #..print message to user interface..
  tot_f = length(files)
  print(paste0("Parsing file names in ", tot_f, " files."))

  #..get file name for each file..
  names = unlist(lapply(files, get_name))

  #..print message with resume of results to user interface..
  print("Finished.")

  return(names)
}
