#' @title Detect Multi-copy Files
#'
#' @description
#' Detect files with multiple copies based on file full name (name+extension) and size.
#'
#' @param fullnames String vector with file full names.
#' @param sizes Numeric Vector with file sizes.
#'
#' @return Matrix with 2 columns: one containing group indexes for files with same full name, and another containing group indexes for files that further have the same size. Files not matching the criteria are given the numeric index 0.
#' @export
#'
#' @examples
#' fullnames = c("CopywriteR.log", "CopywriteR.log", "CopywriteR.log", "resume.txt")
#' sizes = c(4, 4, 100, 20)
#' detect_multicopy_files(fullnames, sizes)

detect_multicopy_files = function(fullnames, sizes){

  #..validate inputs..
  if(!is.character(fullnames)){
    stop("Input must be a character.")
  }
  if(!is.numeric(sizes)){
    stop("Input must be a numeric.")
  }
  if(length(fullnames)!=length(sizes)){
    stop("Inputs must have the same length.")
  }


  # Step 1: detect files with same full name, and given them a common numeric index
  #..print initialization message to user interface..
  tot_f = length(fullnames)
  print(paste0("Detecting files with same full name (name+extension) in a set of ", tot_f, " files."))

  #..detect..
  fullnames_midx = detect_same(fullnames)


  # Step2: detect same full name files with same size
  #..print initialization message to user interface..
  tot_fullnames_midx = sum(fullnames_midx>0)
  print(paste0("Detecting files with same size in a set of ", tot_fullnames_midx, " files."))

  #..detect..
  fullnames_sz_midx = detect_same2(sizes, fullnames_midx)


  #..prepare output..
  multi_idx = cbind(fullnames_midx, fullnames_sz_midx)

  #..print message with resume of results to user interface..
  multi_tot = sum(multi_idx>0)
  multi_perc = signif(multi_tot/tot_f*100, digits=3)
  print(paste0("Total multi-copy detected: ", multi_tot, " files (", multi_perc ,"% of total) under ", max(multi_idx), " groups."))

  return(multi_idx)
}
