#' @title Detect Files With Same Full Name (Name+Extension) And Size
#'
#' @description
#' Detect files sharing the same full name and size and give them a group index.
#'
#' @param sizes Numeric vector with file sizes.
#' @param fullname_midx Numeric vector with group indexes indicating file sizes to be compared against each other.
#'
#' @return Numeric vector with indexes for groups of files with the same full name and size value.
#'
#' @examples
#' detect_same_fullname_size(c(45, 5, 33, 657, 45, 67), c(1 ,0 ,0 ,0 ,1 ,1))

detect_same_fullname_size = function(sizes, fullname_midx){

  #..initialize output variable..
  tot_f = length(sizes)
  fullname_sz_midx = rep(0, tot_f)

  #..for each file full name, flag all files with also have the same size..
  c_idx = 1#..initialize group index for multi-copy candidate files..
  fullname_sz_midx = rep(0, tot_f)
  tot_fullname_midx = max(fullname_midx)

  #..initialize progress bar..
  pb = get_progressbar(tot_fullname_midx)

  #..check if sizes of files..
  for(i in 1:tot_fullname_midx){

    #..separate data for the current file name..
    pos_flag = fullname_midx%in%i#..flag files..
    sizes_fnmulti = sizes[pos_flag]#..get file sizes..

    #..initialize variables for current block of data..
    tot_block = length(sizes_fnmulti)
    c_fullname_sz_midx = rep(0, tot_block)

    #..check which files (for the current file name) have the same size..
    for(i2 in 1:tot_block){
      if(c_fullname_sz_midx[i2]==0){#..entry not analyzed yet..
        data_sizes_flag = sizes_fnmulti%in%sizes_fnmulti[i2]#..check which entries have same size as current entry..
        if(sum(data_sizes_flag)>1){#..case multiple files with same size are detected..
          c_fullname_sz_midx[data_sizes_flag] = c_idx#..given them a index..
          c_idx = c_idx + 1#..update index
        }
      }
    }
    fullname_sz_midx[pos_flag] = c_fullname_sz_midx#..join results from current block of data to the full set of results..

    pb$tick()#..update progress bar..
  }

  return(fullname_sz_midx)
}
