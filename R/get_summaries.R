#' @title Data Summary
#'
#' @description
#' Compute data summaries for multiple tags.
#'
#' @param sizes Numeric vector with sizes.
#' @param tags String vector with tags, one per size value.
#'
#' @return Data summaries per unique tag as computed per R function "summary()": Min., 1st Qu.,  Median, Mean, 3rd Qu. and  Max.
#' @export
#'
#' @examples
#' get_summaries(c(10, 10, 33333, 3, 12), c("txt","txt","bam","txt","bam"))

get_summaries = function(sizes, tags){

  tags_u = unique(tags)#..get unique tags..

  ##..initialize progressbar..
  #pb = get_progressbar(length(tags_u))
  #pb$tick()

  #..get summary for all tags..
  summaries = lapply(tags_u, get_summary, sizes, tags)

  #..format output..
  col_names = names(unlist(summaries[1]))
  summaries = unlist(summaries)
  summaries = matrix(summaries, ncol=6, byrow=TRUE)
  colnames(summaries) = col_names
  rownames(summaries) = tags_u

  return(summaries)
}
