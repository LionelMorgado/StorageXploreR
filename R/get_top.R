#' @title Top Ranking
#'
#' @description
#' Retrieve categories with top values.
#'
#' @param sizes Numeric vector with sizes.
#' @param tags String with tags for each size value.
#' @param max_tags Number of top elements to return.
#'
#' @return Numeric vector with top values sorted is descending order named by tag.
#' @export
#'
#' @examples
#' get_top(c(10, 100, 34, 22, 450), c("txt", "gz", "pdf", "rdata", "bam"), 3)

get_top = function(sizes, tags, max_tags){

  tot_sz = length(sizes)
  if(max_tags>tot_sz){#..top number is higher than values available..
    max_tags = tot_sz#..use all data..
  }

  #..get top values..
  sz_sorted = sort(sizes, decreasing=TRUE, index.return=TRUE)
  sz_sorted_names = tags[sz_sorted$ix[1:max_tags]]
  sz_sorted = sz_sorted$x[1:max_tags]
  names(sz_sorted) = sz_sorted_names

  return(sz_sorted)
}
