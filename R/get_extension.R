#' @title Get File Extension (Single File)
#'
#' @description
#' Get extension for one input file, assuming that the extension follows a standard structure where the extension is the last piece of the file name after a dot.
#'
#' @param dir_fullfn Directories and full file name (file name + extension) to parse.
#'
#' @return Vector of extensions with parsed extensions.
#'
#' @examples
#' get_extension("MyAmazinfFile.pdf"))

get_extension = function(dir_fullfn){

  #..validate inputs..
  if(!is.character(dir_fullfn)){
    stop("Input argument 'dir_fullfn' must be of type character.")
  }

  #..parse..
  dir_fullfn = unlist(strsplit(dir_fullfn, "/"))#..separate the file name from the file directory..
  file_name = dir_fullfn[length(dir_fullfn)]#..get the last tag..
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
