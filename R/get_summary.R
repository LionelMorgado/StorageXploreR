#' @title Data Summary (Single Category)
#'
#' @description
#' Compute data summary for single tag.
#'
#' @param target_tag String for which to compute the data summary.
#' @param sizes Numeric vector with sizes.
#' @param tags String vector with a set of tags.
#'
#' @return Data summary for one tag as compute per R function "summary()": Min., 1st Qu.,  Median, Mean, 3rd Qu. and  Max.
#' @export
#'
#' @examples
#' get_summary("bam", c(10, 10, 33333, 3, 12), c("txt","txt","bam","txt","bam"))

get_summary = function(target_tag, sizes, tags){

  c_filter = tags%in%target_tag
  target_tag_summary = summary(sizes[c_filter])

  return(target_tag_summary)
}
