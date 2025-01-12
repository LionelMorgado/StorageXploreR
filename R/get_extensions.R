#' @title Get File Extension
#'
#' @description
#' Get file extension from a set of strings assuming that the file naming follows a structure where the extension is the last piece of the file full name after a dot.
#'
#' @param files String vector containing for files the path and full name.
#'
#' @return String vector with file extensions.
#' @export
#'
#' @examples
#' get_extensions(c("./CNV/CNAprofiles_HUB.01.B2.001/CopywriteR.log", "./Results/resume.txt"))
#' get_extensions("MyAmazinfFile.pdf")

get_extensions = function(files){

  #..print initialization message to user interface..
  tot_f = length(files)
  print(paste0("Parsing file extensions for ", tot_f, " files."))

  #..get extension for each file..
  extension = unlist(lapply(files, get_extension))

  #..print message with resume of results to user interface..
  tot_xt = sum(extension!="NotDetected")
  xt_perc = signif(tot_xt/tot_f*100, digits=3)
  print(paste0("File extension detected for ", tot_xt, " files (", xt_perc ,"% of total)."))

  return(extension)
}
