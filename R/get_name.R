#' @title Get File Name (Single File)
#'
#' @description
#' Get file name (file name without extension) from a full file name tag. Assumes that the tag follows the format: <file_name>.<file_extension>.
#'
#' @param fullfn String to be parsed.
#'
#' @return File name without file extension.
#' @export
#'
#' @examples
#' get_name("myfile.txt")

get_name = function(fullfn){

  fn = get_fullname(fullfn)
  fn = unlist(strsplit(fn, "\\."))[1]

  return(fn)
}
