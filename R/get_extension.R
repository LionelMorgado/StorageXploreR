#' @title Get File Extension (Single File)
#'
#' @description
#' Get file extension a string assuming that the file naming follows a structure where the extension is the last piece of the file full name after a dot.
#'
#' @param file Directories and full file name (file name + extension) to parse.
#'
#' @return String with file extension.
#'
#' @examples
#' get_extension("./CNV/CNAprofiles_HUB.01.B2.001/CopywriteR.log")
#' get_extension("MyAmazinfFile.pdf"))

get_extension = function(file){

  #..validate inputs..
  if(!is.character(file)){
    stop("Input argument 'file' must be of type character.")
  }

  #..parse..
  file = unlist(strsplit(file, "/"))#..separate the file name from the file directory..
  file_name = file[length(file)]#..get the last tag..
  xt_flag = length(grep("\\.", file_name))>0#..check if there is a dot in the last tag..
  if(xt_flag==TRUE){#..dot detected..
    extension = unlist(strsplit(file_name, "\\."))#..split tag..
    extension = extension[length(extension)]#..get the tag after the last dot..
    extension = tolower(extension)#..convert all extension to lower case..
  }else{
    extension = "NotDetected"
  }

  return(extension)
}
