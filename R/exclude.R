#' @title Exclude Data
#'
#' @description
#' Detects tags in a set starting with a given exclude tag.
#'
#' @param tags String vector with tags to be filtered.
#' @param exclude_tags String vector with tags to exclude from the dataset.
#'
#' @return Bolean flag with FALSE for items to be excluded, and TRUE otherwise.
#' @export
#'
#' @examples
#' exclude(c("asdttttttt", "sssasd", "asd", "fff"), c("asd","fffffff"))

exclude = function(tags, exclude_tags=NULL){

  #..validate inputs..
  if(is.null(exclude_tags)){
    stop("No exclusion set provided.")
  }
  if(!is.character(tags)){
    stop("Input argument 'tags' must be of type character.")
  }
  if(!is.character(exclude_tags)){
    stop("Input argument 'exclude_tags' must be of type character.")
  }

  if(is.null(exclude_tags)){#..user did not provide an inclusion set..
    print("Aborting: no inclusion set provided.")
  }else{

    #..initialize output variable..
    tot_tags = length(tags)
    exc_flag = rep(TRUE, tot_tags)#..flag for exclusion..

    #..flag position of directories to exclude in the analysis in the full set..
    tot_exc = length(exclude_tags)
    for(i in 1:tot_exc){
      c_exc = exclude_tags[i]#..get current tag..
      c_exc_len = nchar(c_exc)
      tags_short = substr(tags, 1, c_exc_len)#..shorten all directories to target length..
      exc_filter = tags_short%in%c_exc#..check which directories contain the exclude tag..
      exc_flag[exc_filter] = FALSE
    }

    #..print message with resume of results to user interface..
    exc_perc = signif(sum(exc_flag)/length(exc_flag)*100, digits=3)
    print(paste0("Total excluded: ", sum(exc_flag), " (", exc_perc, "% of total)"))

    return(exc_flag)
  }
}
