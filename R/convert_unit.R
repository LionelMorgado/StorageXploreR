#' @title Data Size Unit Conversion
#'
#' @description
#' Convert storage size data to different units.
#'
#' @param size Data size.
#' @param in_unit Input data unit.
#' @param out_unit Output data unit. This can be set to "AUTO" for automatic optimization, which is useful for plotting.
#'
#' @return Converted data size to output unit.
#' @export
#'
#' @examples
#' convert_unit(10000, "KB", "MB")
#' convert_unit(15007, "KB", "AUTO")

convert_unit = function(size, in_unit=c("B", "KB", "MB", "GB", "TB", "PB"), out_unit=c("AUTO", "B", "KB", "MB", "GB", "TB", "PB")){

  #..define size units recognized by the system by ascending order..
  all_units = c("B", "KB", "MB", "GB", "TB", "PB")

  # Match argument
  in_unit = match.arg(in_unit)
  out_unit = match.arg(out_unit)

  in_pos = which(all_units%in%in_unit)#..get position of input unit in the list of recognized units..
  if(out_unit=="AUTO"){#..automatically detect best convertion unit..
    #..detect best unit to convert based on the number of digits of the maximum value in the input data..
    print(paste0("Determining best convertion unit."))
    sz_max = max(size)
    tot_int_digits = nchar(trunc(sz_max))
    scale_factor = ceiling(tot_int_digits/3)-1
    out_pos = in_pos+scale_factor

    #..get tag for the new unit..
    out_unit = all_units[out_pos]

    #..print update message to user interface..
    print(paste0("Converting units: ", in_unit, " -> ", out_unit, "."))

  }else{#..convert to user defined unit..

    #..print update message to user interface..
    print(paste0("Converting units: ", in_unit, " -> ", out_unit, "."))

    #..get position of output unit in the list of recognized units..
    out_pos = which(all_units%in%out_unit)
  }

  #..convert data to new unit..
  adj_factor = (out_pos-in_pos)*3
  size = size/10^adj_factor
  names(size) = out_unit

  return(size)
}
