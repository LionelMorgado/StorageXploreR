#' @title Compute Totals For Sets
#'
#' @description
#' Calculate total from a set of values filtered by corresponding tag.
#'
#' @param tag Tag for which to compute totals.
#' @param sizes Vector with values per tag.
#' @param tags Vector with all tags for which a value is provided.
#'
#' @return Total value.
#'
#' @examples
#' get_size("txt", c(10, 50, 10, 33, 77), c("txt", "txt", "sz", "bam", "txt"))

get_size = function(tag,  sizes, tags){

  c_filter = tags%in%tag#..filter tags..
  size = sum(sizes[c_filter])#..compute total size..

  return(size)
}
