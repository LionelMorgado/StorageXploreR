#' @title Data Summary (Single Category)
#'
#' @description
#' Compute data summary for single tag.
#'
#' @param target_id Tag for which to compute the data summary.
#' @param sizes Vector with numbers.
#' @param ids Vector with tags.
#'
#' @return Data summary for one tag as compute per R function "summary()": Min., 1st Qu.,  Median, Mean, 3rd Qu. and  Max.
#' @export
#'
#' @examples
#' get_summary("bam", c(10, 10, 33333, 3, 12), c("txt","txt","bam","txt","bam"))

get_summary = function(target_id, sizes, ids){

  c_filter = ids%in%target_id
  ids_summary = summary(sizes[c_filter])

  return(ids_summary)
}
