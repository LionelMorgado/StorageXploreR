#' @title Get File Name (Single File)
#'
#' @description
#' Get file name (name without extension) from a file full name tag. Assumes that the tag follows the format: <name>.<extension>.
#'
#' @param file String containing file full name, concatenated or not with the path.
#'
#' @return File name without extension.
#' @export
#'
#' @examples
#' get_name("myfile.txt")

get_name = function(file){

  fn = get_fullname(file)
  fn = unlist(strsplit(file, "\\."))[1]

  return(fn)
}
