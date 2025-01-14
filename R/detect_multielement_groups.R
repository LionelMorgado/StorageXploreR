#' @title Detect Multi-Element Groups
#'
#' @description
#' Detect groups with multiple elements and give them a common index number.
#'
#' @param elements String vector with tags.
#'
#' @return Vector with numeric group indexes.
#' @export
#'
#' @examples
#' detect_multielement_groups(c("asd.txt","asd.Rdata", "qwe.txt","rrrrrrrr.txt","asd.txt", "asd.txt"))

detect_multielement_groups = function(tags){

  #..get counts for each tag..
  tags = tolower(tags)#..ignore case..
  tags_counts = table(tags)#..get tags counts..
  tags_multi_flag = tags_counts>1#..detect tags with multiple copies..

  #..create an index for all tags which are equal..
  tot_tags_multi = sum(tags_multi_flag)
  if(tot_tags_multi>0){

    #..create a set with unrepeated tags..
    tags_multi_u = names(tags_counts)[tags_multi_flag]

    #..initialize output variable..
    tot_data = length(tags)
    tags_midx = rep(0, tot_data)

    #..initialize progress bar..
    pb = get_progressbar(tot_tags_multi)

    #..detect equal tags in the full list and give them a common index..
    for(i in 1:tot_tags_multi){

      c_flag = tags%in%tags_multi_u[i]#..detect tags..
      tags_midx[c_flag] = i#..give index..

      pb$tick()#..update progress bar..
    }

    return(tags_midx)
  }
}
