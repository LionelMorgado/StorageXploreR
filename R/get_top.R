#' @title Top Ranking
#'
#' @description
#' Retrieve categories with top values.
#'
#' @param sizes Vector of values.
#' @param ids Identifier tags for each input value.
#' @param max_ids Number of top elements to return.
#'
#' @return Vector with top values sorted is descending order.
#' @export
#'
#' @examples
#' get_top(c(10, 100, 34, 22, 450), c("txt", "gz", "pdf", "rdata", "bam"), 3)

get_top = function(sizes, ids, max_ids){

  tot_sz = length(sizes)
  if(max_ids>tot_sz){#..top number is higher than values available..
    max_ids = tot_sz#..use all data..
  }

  #..get top values..
  sz_sorted = sort(sizes, decreasing=TRUE, index.return=TRUE)
  sz_sorted_names = ids[sz_sorted$ix[1:max_ids]]
  sz_sorted = sz_sorted$x[1:max_ids]
  names(sz_sorted) = sz_sorted_names

  return(sz_sorted)
}
