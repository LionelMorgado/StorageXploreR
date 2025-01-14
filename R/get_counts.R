#' @title Count Total
#'
#' @description
#' Calculate total number of instances in a set.
#'
#' @param tags String vector with tags for which to get counts.
#' @param target_tags String vector with tags to keep.
#'
#' @return Table with counts for unique tags.
#' @export
#'
#' @examples
#' get_counts(c("txt", "gz", "txt", "gz", "txt", "bam"))

get_counts = function(tags, target_tags=NULL){

  counts = table(tags)#..count occurrences of tags..
  if(!is.null(target_tags)){#..keep only data for user provided tags..
    target_flag = names(counts)%in%target_tags
    counts = counts[target_flag]
  }

  return(counts)
}
