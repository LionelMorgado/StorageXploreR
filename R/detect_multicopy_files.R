#' @title Detect Multi-copy Files
#'
#' @description
#' Detect files with multiple copies based on file full name (name+extension) and size.
#'
#' @param files String vector with files path and full name.
#' @param sizes Numeric Vector with file sizes.
#' @param use_md5sum Bollean indicating if md5sum codes must be used or not.
#'
#' @return Matrix with 2 columns: one containing group indexes for files with same full name, and another containing group indexes for files that further have the same size. Files not matching the criteria are given the numeric index 0.
#' @export
#'
#' @examples
#' files = rep("D:/Stuff/GITHUBPortfolio/Storage/data/mitra/known_db.rda", 4)
#' sizes = c(100, 4, 10, 4)
#' detect_multicopy_files(fullnames, sizes)

detect_multicopy_files = function(files, sizes, use_md5sum=FALSE){

  #..validate inputs..
  if(!is.character(files)){
    stop("Input must be a character.")
  }
  if(!is.numeric(sizes)){
    stop("Input must be a numeric.")
  }
  if(length(files)!=length(sizes)){
    stop("Inputs must have the same length.")
  }


  # Step 1: detect files with same full name, and given them a common numeric index
  #..print initialization message to user interface..
  tot_f = length(files)
  print(paste0("Detecting files with same full name (name+extension) in a set of ", tot_f, " files."))

  #..get files full name..
  fullnames = get_fullnames(files)

  #..detect..
  fullnames_midx = detect_multielement_groups(fullnames)


  # Step2: detect same full name files with same size
  #..print initialization message to user interface..
  tot_fullnames_midx = sum(fullnames_midx>0)
  print(paste0("Detecting files with same fullname and size in a set of ", tot_fullnames_midx, " files."))

  #..detect..
  fullnames_sz_midx = detect_multielement_subgroups(sizes, fullnames_midx)

  #..prepare output..
  multi_idx = cbind(fullnames_midx, fullnames_sz_midx)


  # Step3: detect same full name files with same size and same md5sum
  fullnames_sz_midx_flag = fullnames_sz_midx>0
  tot_fullnames_sz_midx = sum(fullnames_sz_midx_flag)
  if((use_md5sum==TRUE) && (tot_fullnames_sz_midx>0)){#..execute md5sum analysis..

    #..print initialization message to user interface..
    print(paste0("Detecting files with same fullname, size and md5sum in a set of ", tot_fullnames_sz_midx, " files."))


    #..compute md5sum..
    c_files = files[fullnames_sz_midx_flag>0]
    c_md5sums = md5sum(c_files)

    #..transpose results for the fullnames_sz_midx set..
    md5sums = rep("", length(fullnames_sz_midx_flag))
    md5sums[fullnames_sz_midx_flag] = c_md5sums


    #..compare md5sum..
    fullnames_sz_md5sum_midx = detect_multielement_subgroups(md5sums, fullnames_sz_midx)


    #..prepare output..
    multi_idx = cbind(multi_idx, fullnames_sz_md5sum_midx)
  }

  #..print message with resume of results to user interface..
  tot_col = ncol(multi_idx)
  multi_tot = sum(multi_idx[, tot_col])
  multi_perc = signif(multi_tot/tot_f*100, digits=3)
  print(paste0("Total multi-copy detected: ", multi_tot, " files (", multi_perc ,"% of total) under ", max(multi_idx[, tot_col]), " groups."))

  return(multi_idx)
}
