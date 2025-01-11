#' @title Count Total
#'
#' @description
#' Calculate total number of instances in a set.
#'
#' @param ids Ids for which to get counts.
#' @param target_ids Ids to keep.
#'
#' @return Table with counts for unique ids.
#' @export
#'
#' @examples
#' get_counts(c("txt", "gz", "txt", "gz", "txt", "bam"))

get_counts = function(ids, target_ids=NULL){

  counts = table(ids)#..count occurrences of ids..
  if(!is.null(target_ids)){#..keep only data for user provided ids..
    target_flag = names(counts)%in%target_ids
    counts = counts[target_flag]
  }

  return(counts)
}
