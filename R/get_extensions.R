#' @title Get File Extension
#'
#' @description
#' Get extension for files, assuming that the extension follows a standard structure where the extension is the last piece of the file name after a dot.
#'
#' @param dir_fullfns Vector with directories and full file names (file name + extension).
#'
#' @return Vector with parsed extensions.
#' @export
#'
#' @examples
#' get_extensions(c("./CNV/CNAprofiles_HUB.01.B2.001/CopywriteR.log", "./Results/resume.txt"))
#' get_extensions("MyAmazinfFile.pdf")

get_extensions = function(dir_fullfns){

  #..print initialization message to user interface..
  tot_f = length(dir_fullfns)
  print(paste0("Parsing file extensions for ", tot_f, " files."))

  #..get extension for each file..
  extension = unlist(lapply(dir_fullfns, get_extension))

  #..print message with resume of results to user interface..
  tot_xt = sum(extension!="NotDetected")
  xt_perc = signif(tot_xt/tot_f*100, digits=3)
  print(paste0("File extension detected for ", tot_xt, " files (", xt_perc ,"% of total)."))

  return(extension)
}
