#' @title Get File Full Name (Single File)
#'
#' @description
#' Get file full name (name+extension) from a string by removing all of the path up to and including the last path separator (if any).
#'
#' @param files String containing for a file the path and full name.
#'
#' @return String with file full name.
#'
#' @examples
#' get_fullname("/mnt/folder1/MyAmazinfFile.pdf")

get_fullname = function(files){

  #..validate inputs..
  if(!is.character(files)){
    stop("Input argument 'files' must be of type character.")
  }

  fullname = unlist(strsplit(files, "/"))
  fullname = fullname[length(fullname)]

  return(fullname)
}
