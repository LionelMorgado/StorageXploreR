#' @title Get File Full Name
#'
#' @description
#' Get file full names (name+extension) from tags composed of directory+name+extension.
#'
#' @param dir_fullfns Vector with tags composed of directory followed by file full name (file name + extension).
#'
#' @return Vector with parsed file full names (file name+extension).
#'
#' @export
#'
#' @examples
#' get_fullnames(c("./CNV/CNAprofiles_HUB.01.B2.001/CopywriteR.log", "./Results/resume.txt"))
#' get_fullnames("MyAmazinfFile.pdf")

get_fullnames = function(dir_fullfns){

  #..print message to user interface..
  tot_f = length(dir_fullfns)
  print(paste0("Parsing full file names (name+extension) for ", tot_f, " files."))

  #..get full name for each file..
  fullnames = unlist(lapply(dir_fullfns, get_fullname))

  #..print message with resume of results to user interface..
  print("Finished.")

  return(fullnames)
}
