#' @title Detect Related Files
#'
#' @description
#' Detect related files (criteria: same name but extension can differ).
#'
#' @param files String vector containing for files the path and full name.
#'
#' @return Matrix with group indexes for related files.
#' @export
#'
#' @examples
#' files = c("./myfolder/CopywriteR.log", "CopywriteR.log", "./dir/resume.txt" , "./dif_dir/difsubdir/resume.txt", "single.txt", "CopywriteR.log")
#' detect_related_files(files)

detect_related_files = function(files){

  #..validate inputs..
  if(!is.character(files)){
    stop("Input must be a character.")
  }

  #..print initialization message to user interface..
  tot_f = length(files)
  print(paste0("Detecting related files (same name but extension can differ) in a set of ", tot_f, " files."))

  #..get files with the same name..
  names = get_names(files)
  names = tolower(names)#..remove case variation..

  #..determine file names with multiple occurrences..
  names_counts = table(names)
  names_multi_flag = names_counts>1

  #..initialize output variables
  related_idx = rep(0, tot_f)

  tot_names_multi = sum(names_multi_flag)
  if(tot_names_multi>0){#..in the case there are repeated file names..

    #..print initialization message to user interface..
    print(paste0("Assigning group index to a set of ", tot_names_multi, " files."))

    #..initialize progress bar..
    pb = get_progressbar(tot_names_multi)

    #..create a group index for related files..
    names_multi_u = names(names_counts)[names_multi_flag]#..get unrepeated file names..
    for(i in 1:tot_names_multi){
      c_flag = names%in%names_multi_u[i]#..detect file position in the full set..
      related_idx[c_flag] = i#..assign group index..

      pb$tick()#..update progress bar..
    }
  }

  #..print message with resume of results to user interface..
  related_tot = sum(related_idx>0)
  related_perc = signif(related_tot/tot_f*100, digits=3)
  print(paste0("Total detected: ", related_tot, " files (", related_perc ,"% of total) under ", max(related_idx), " groups."))

  return(related_idx)
}
