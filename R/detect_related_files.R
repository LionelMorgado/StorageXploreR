#' @title Detect Related Files
#'
#' @description
#' Detect related files (criteria: same name but extension can differ).
#'
#' @param all_fullfn Vector with full file names (file name + extension).
#'
#' @return Matrix with group indexes for related files.
#' @export
#'
#' @examples
#' fullfn = c("./myfolder/CopywriteR.log", "CopywriteR.log", "./dir/resume.txt" , "./dif_dir/difsubdir/resume.txt", "single.txt", "CopywriteR.log")
#' detect_related_files(fullfn)

detect_related_files = function(all_fullfn){

  #..validate inputs..
  if(!is.character(all_fullfn)){
    stop("Input must be a character.")
  }

  #..print initialization message to user interface..
  tot_f = length(all_fullfn)
  print(paste0("Detecting related files (same name but extension can differ) in a set of ", tot_f, " files."))

  #..get files with the same name..
  all_fn = get_names(all_fullfn)
  all_fn = tolower(all_fn)#..remove case variation..

  #..determine file names with multiple occurrences..
  fn_counts = table(all_fn)
  fn_multi_flag = fn_counts>1

  #..initialize output variables
  tot_data = length(all_fullfn)
  related_idx = rep(0, tot_data)

  tot_fn_multi = sum(fn_multi_flag)
  if(tot_fn_multi>0){#..in the case there are repeated file names..

    #..print initialization message to user interface..
    print(paste0("Assign group numeric index to a set of ", tot_fn_multi, " related files."))

    #..initialize progress bar..
    pb = get_progressbar(tot_fn_multi)

    #..create a group index for related files..
    fn_multi_u = names(fn_counts)[fn_multi_flag]#..get unrepeated file names..
    for(i in 1:tot_fn_multi){
      c_flag = all_fn%in%fn_multi_u[i]#..detect file position in the full set..
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
