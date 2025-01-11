#' @title Compute Totals For Sets (Fast Version)
#'
#' @description
#' Calculate total from a set of values filtered by corresponding tag.
#'
#' @param sizes Vector with values per tag.
#' @param tags Vector with all tags for which a value is provided.
#'
#' @return Vector with the totals for each tag.
#' @export
#'
#' @examples
#' get_sizes(c(10, 50, 10, 33, 77), c("txt", "txt", "sz", "bam", "txt"))

get_sizes = function(sizes, tags){
  tags_u = unique(tags)
  sizes = unlist(lapply(tags_u, get_size, sizes, tags))
  names(sizes)=tags_u

  return(sizes)
}
