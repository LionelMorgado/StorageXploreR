#' @title Detect Files With Same Full Name (Name+Extension)
#'
#' @description
#' Detect files with same full name (name+extension) and give them a common index number.
#'
#' @param all_fullfn Vector with full file names (file name + extension).
#'
#' @return Vector with numeric indexes for groups of files with the same full name.
#' @export
#'
#' @examples
#' detect_same(c("asd.txt","asd.Rdata", "qwe.txt","rrrrrrrr.txt","asd.txt", "asd.txt"))

detect_same = function(all_fullfn){
  #..get counts for each file..
  all_fullfn = tolower(all_fullfn)#..ignore case..
  fullfn_counts = table(all_fullfn)#..get file counts..
  fullfn_multi_flag = fullfn_counts>1#..detect files with multiple copies..

  #..create an index for all files with the same name..
  tot_fullfn_multi = sum(fullfn_multi_flag)
  if(tot_fullfn_multi>0){

    #..create a set with unrepeated file names..
    fullfn_multi_u = names(fullfn_counts)[fullfn_multi_flag]

    #..initialize output variable..
    tot_data = length(all_fullfn)
    fullfn_midx = rep(0, tot_data)

    #..initialize progress bar..
    pb = get_progressbar(tot_fullfn_multi)

    #..detect files with same name in the full list of files and give them a common index..
    for(i in 1:tot_fullfn_multi){

      c_flag = all_fullfn%in%fullfn_multi_u[i]#..detect files..
      fullfn_midx[c_flag] = i#..give index..

      pb$tick()#..update progress bar..
    }

    return(fullfn_midx)
  }
}
