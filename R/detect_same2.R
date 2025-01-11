#' @title Detect Files With Same Full Name (Name+Extension) And Size
#'
#' @description
#' Detect files (sharing the same full name) which have the same size and give them a common index number.
#'
#' @param size Vector with file sizes.
#' @param fullfn_midx Vector with numeric indexes for which size values to comapre against each other.
#'
#' @return Vector with numeric indexes for groups with the same full size value.
#'
#' @examples
#' detect_same2(c(45, 5, 33, 657, 45, 67), c(1 ,0 ,0 ,0 ,1 ,1))

detect_same2 = function(size, fullfn_midx){

  #..initialize output variable..
  tot_f = length(size)
  fullfn_sz_midx = rep(0, tot_f)

  #..for each file full name, flag all files with also have the same size..
  c_idx = 1#..initialize group index for multi-copy candidate files..
  fullfn_sz_midx = rep(0, tot_f)
  tot_fullfn_midx = max(fullfn_midx)

  #..initialize progress bar..
  pb = get_progressbar(tot_fullfn_midx)

  #..check if sizes of files..
  for(i in 1:tot_fullfn_midx){

    #..separate data for the current file name..
    pos_flag = fullfn_midx%in%i#..flag files..
    size_fnmulti = size[pos_flag]#..get file sizes..

    #..initialize variables for current block of data..
    tot_block = length(size_fnmulti)
    c_fullfn_sz_midx = rep(0, tot_block)

    #..check which files (for the current file name) have the same size..
    for(i2 in 1:tot_block){
      if(c_fullfn_sz_midx[i2]==0){#..entry not analyzed yet..
        data_size_flag = size_fnmulti%in%size_fnmulti[i2]#..check which entries have same size as current entry..
        if(sum(data_size_flag)>1){#..case multiple files with same size are detected..
          c_fullfn_sz_midx[data_size_flag] = c_idx#..given them a index..
          c_idx = c_idx + 1#..update index
        }
      }
    }
    fullfn_sz_midx[pos_flag] = c_fullfn_sz_midx#..join results from current block of data to the full set of results..

    pb$tick()#..update progress bar..
  }

  return(fullfn_sz_midx)
}
