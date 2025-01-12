#' @title Determine File Format (Single File)
#'
#' @description
#' Determine file format from extension.
#'
#' @param extensions String vector with file extensions.
#'
#' @return String vector with all possible extensions for the detected format, for each file. File extensions wich are not recognized (and for which a format cannot be determined) are assigned the tag "Unknown".
#'
#' @examples
#' get_format(c("htm", "txt","random_tag"))
#' get_format("mpg")

get_format = function(extensions){

  #..validate inputs..
  if(!is.character(extensions)){
    stop("Input argument 'extension' must be of type character.")
  }

  #..load list of known file extensions and formats..
  data(known_db)#..load data..
  known_db_u = unique(unlist(strsplit(known_db[,1], ", ")))#..because file extensions are grouped by category and may be repeated in the list, separate them and get unique tags..

  #..initialize output variable..
  tot_data = length(extensions)
  all_fm = rep("", tot_data)

  #..start processing files with unknown extension..
  knownxt_flag = extensions%in%known_db_u#..detect files without known extension..
  all_fm[!knownxt_flag] = "Unknown"#..assign "Unknown" format to files for which no extension was detected..

  #..process files with known extension..
  xt_known = extensions[knownxt_flag]#..separate known extensions detected..
  xt_known_u = unique(extensions[knownxt_flag])#..make set of unique tags..
  known_db2 = paste0(", ", known_db[,1], ",")#..prepare tags to be used in search..

  #..get the file format for all input files by mapping file extensions to the set of known extensions..
  tot_xt_u = length(xt_known_u)
  for(i in 1:tot_xt_u){
    pos_in_data = which(extensions%in%xt_known_u[i])#..detect all entries with same extension in the input file set..
    c_xt = paste(", ", xt_known_u[i], ",",sep="")#..prepare format for seach..
    pos_in_known = grep(c_xt, known_db2)#..search file extension in list of known formats..
    all_fm[pos_in_data] = known_db[pos_in_known[1], 1]#..some extensions can show multiple times in the known list, therefore only the first appearance is used...
  }

  return(all_fm)
}
