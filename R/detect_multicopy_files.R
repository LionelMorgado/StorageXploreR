#' @title Detect Multi-copy Files
#'
#' @description
#' Detect files with multiple copies based on full file name (filename+extension) and size.
#'
#' @param all_fullfn Vector with full file names (file name + extension).
#' @param size Vector with size of files.
#'
#' @return Matrix with 2 columns: one containing group indexes for files with same name, and another containing group indexes for files that further have the same size. Files not matching the criteria are given the numeric index 0.
#' @export
#'
#' @examples
#' fullfn = c("CopywriteR.log", "CopywriteR.log", "CopywriteR.log", "resume.txt")
#' sizes = c(4, 4, 100, 20)
#' detect_multicopy_files(fullfn, sizes)

detect_multicopy_files = function(all_fullfn, size){

  #..validate inputs..
  if(!is.character(all_fullfn)){
    stop("Input must be a character.")
  }
  if(!is.numeric(size)){
    stop("Input must be a numeric.")
  }
  if(length(all_fullfn)!=length(size)){
    stop("Inputs must have the same length.")
  }


  # Step 1: detect files with same full name, and given them a common numeric index
  #..print initialization message to user interface..
  tot_f = length(all_fullfn)
  print(paste0("Detecting files with same full name (name+extension) in a set of ", tot_f, " files."))

  #..detect..
  fullfn_midx = detect_same(all_fullfn)


  # Step2: detect same full name files with same size
  #..print initialization message to user interface..
  tot_fullfn_midx = sum(fullfn_midx>0)
  print(paste0("Detecting files with same size in a set of ", tot_fullfn_midx, " files with same full name (name+extension)."))

  #..detect..
  fullfn_sz_midx = detect_same2(size, fullfn_midx)


  #..prepare output..
  multi_idx = cbind(fullfn_midx, fullfn_sz_midx)

  #..print message with resume of results to user interface..
  multi_tot = sum(multi_idx>0)
  multi_perc = signif(multi_tot/tot_f*100, digits=3)
  print(paste0("Total multi-copy detected: ", multi_tot, " files (", multi_perc ,"% of total) under ", max(multi_idx), " groups."))

  return(multi_idx)
}
