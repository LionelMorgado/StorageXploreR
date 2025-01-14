#' @title Scan Storage
#'
#' @description
#' Get name and size for all files in a user defined root directory and subdirectories.
#'
#' @param data_dir Directory where to scan for files.
#' @param save_dir Directory where to save the results from the storage scan.
#'
#' @export
#'
#' @examples
#' scan_storage(data_dir="/mnt/d/", save_dir="/mnt/d/results/")
#' scan_storage(data_dir="c:/ProjectX/Data/")

scan_storage = function(data_dir=NULL, save_dir=NULL){

  #..validate input..
  if (is.character(data_dir)!=TRUE){
    stop("'data_dir' argument is not valid.")
  }

  #..check if data directory exists..
  if(dir.exists(data_dir)!=TRUE){
    stop("Directory does not exist.")
  }

  #..define output directory..
  if(is.null(save_dir)){#..no output directory was provided..
    save_dir = data_dir#..save results in the data directory..
  }

  #..define output file..
  c_date = paste0(Sys.Date(), "_", format(Sys.time(), "%Hh%Mm"))
  s_name = paste0(save_dir, "StorageXploreR-", c_date, ".txt")

  #..get file names..
  print("Scanning storage for files")#..print initialization message to user interface..
  file_names = list.files(data_dir, full.names=TRUE, recursive = TRUE)

  #..get file sizes..
  print(paste0("Getting sizes for ", length(file_names), " files."))#..print initialization message to user interface..
  sizes = file.size(file_names)

  #..prepare output..
  dataset = cbind(sizes, file_names)#..join data..
  colnames(dataset) = c("B", "File")#..add header..

  #..save..
  write.table(dataset, s_name, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

  #..print update message to user interface..
  print(paste0("Printing output to ", s_name, "."))
  print("Finished!")
}
