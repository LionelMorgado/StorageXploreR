#' @title Data Summary
#'
#' @description
#' Compute data summaries for multiple tags.
#'
#' @param sizes Vector with numbers.
#' @param ids Vector with tags.
#'
#' @return Data summaries per unique tag as compute per R function "summary()": Min., 1st Qu.,  Median, Mean, 3rd Qu. and  Max.
#' @export
#'
#' @examples
#' get_summaries(c(10, 10, 33333, 3, 12), c("txt","txt","bam","txt","bam"))

get_summaries = function(sizes, ids){

  ids_u = unique(ids)#..get unique ids..

  ##..initialize progressbar..
  #pb = get_progressbar(length(ids_u))
  #pb$tick()

  #..get summary for each ids..
  summaries = lapply(ids_u, get_summary, sizes, ids)

  #..format output..
  col_names = names(unlist(summaries[1]))
  summaries = unlist(summaries)
  summaries = matrix(summaries, ncol=6, byrow=TRUE)
  colnames(summaries) = col_names
  rownames(summaries) = ids_u

  return(summaries)
}
