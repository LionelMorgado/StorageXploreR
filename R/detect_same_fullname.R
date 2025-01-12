#' @title Detect Files With Same Full Name (Name+Extension)
#'
#' @description
#' Detect files with same full name (name+extension) and give them a common index number.
#'
#' @param fullnames String vector with file full names (name+extension).
#'
#' @return Vector with numeric indexes for groups of files with the same full name.
#' @export
#'
#' @examples
#' detect_same_fullname(c("asd.txt","asd.Rdata", "qwe.txt","rrrrrrrr.txt","asd.txt", "asd.txt"))

detect_same_fullname = function(fullnames){
  #..get counts for each file..
  fullnames = tolower(fullnames)#..ignore case..
  fullnames_counts = table(fullnames)#..get file counts..
  fullnames_multi_flag = fullnames_counts>1#..detect files with multiple copies..

  #..create an index for all files with the same name..
  tot_fullnames_multi = sum(fullnames_multi_flag)
  if(tot_fullnames_multi>0){

    #..create a set with unrepeated file names..
    fullnames_multi_u = names(fullnames_counts)[fullnames_multi_flag]

    #..initialize output variable..
    tot_data = length(fullnames)
    fullnames_midx = rep(0, tot_data)

    #..initialize progress bar..
    pb = get_progressbar(tot_fullnames_multi)

    #..detect files with same name in the full list of files and give them a common index..
    for(i in 1:tot_fullnames_multi){

      c_flag = fullnames%in%fullnames_multi_u[i]#..detect files..
      fullnames_midx[c_flag] = i#..give index..

      pb$tick()#..update progress bar..
    }

    return(fullnames_midx)
  }
}
