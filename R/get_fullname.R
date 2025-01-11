#' @title Get File Full Name (Single File)
#'
#' @description
#' Get file full name (name+extension) from one tag composed of file directory+name+extension.
#'
#' @param dir_fullfn String tag composed of directoriy followed by file full name (file name + extension).
#'
#' @return Parsed file full name (file name+extension)).
#'
#' @examples
#' get_fullname("MyAmazinfFile.pdf")

get_fullname = function(dir_fullfn){

  #..validate inputs..
  if(!is.character(dir_fullfn)){
    stop("Input argument 'dir_fullfn' must be of type character.")
  }

  fullfn = unlist(strsplit(dir_fullfn, "/"))
  fullfn = fullfn[length(fullfn)]

  return(fullfn)
}
