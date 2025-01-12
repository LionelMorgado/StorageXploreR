#' @title Get File Full Name
#'
#' @description
#' Get file full name (name+extension) from a set of strings by removing all of the path up to and including the last path separator (if any).
#'
#' @param files String vector containing for files the path and full name.
#'
#' @return Vector of strings with file full names.
#'
#' @export
#'
#' @examples
#' get_fullnames(c("./CNV/CNAprofiles_HUB.01.B2.001/CopywriteR.log", "./Results/resume.txt"))
#' get_fullnames("MyAmazinfFile.pdf")

get_fullnames = function(files){

  #..print message to user interface..
  tot_files = length(files)
  print(paste0("Parsing full file names (name+extension) for ", tot_files, " files."))

  #..get full name for each file..
  fullnames = unlist(lapply(files, get_fullname))

  #..print message with resume of results to user interface..
  print("Finished.")

  return(fullnames)
}
