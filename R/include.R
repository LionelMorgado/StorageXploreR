#' @title Include Data
#'
#' @description
#' Detects tags in a set starting with a given include tag.
#'
#' @param tags String vector with tags to be filtered.
#' @param include_tags String vector with tags to include in the dataset.
#'
#' @return Flag with TRUE bollean value for items to be included.
#' @export
#'
#' @examples
#' include(c("asdttttttt", "sssasd", "asd"), c("asd","fffffff"))

include = function(tags, include_tags=NULL){

  #..validate inputs..
  if(is.null(include_tags)){
    stop("No inclusion set provided.")
  }
  if(!is.character(tags)){
    stop("Input argument 'tags' must be of type character.")
  }
  if(!is.character(include_tags)){
    stop("Input argument 'include_tags' must be of type character.")
  }

  #..initialize output variable..
  tot_tags = length(tags)
  inc_flag = rep(FALSE, tot_tags)#..flag for inclusion..

  #..flag position of directories to include in the analysis in the full set..
  tot_inc = length(include_tags)
  for(i in 1:tot_inc){
    c_inc = include_tags[i]#..get current tag..
    c_inc_len = nchar(c_inc)
    tags_short = substr(tags, 1, c_inc_len)#..shorten all directories to target length..
    inc_filter = tags_short%in%c_inc#..check which directories contain the include tag..
    inc_flag[inc_filter] = TRUE
  }

  #..print message with resume of results to user interface..
  inc_perc = signif(sum(inc_flag)/length(inc_flag)*100, digits=3)
  print(paste0("Total included: ", sum(inc_flag), " (", inc_perc, "% of total)"))

  return(inc_flag)
}
