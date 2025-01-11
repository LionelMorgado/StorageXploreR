#' @title Boxplot
#'
#' @description
#' Boxplot for instance sizes by category.
#'
#' @param sizes Numeric vector with sizes for instances in categories (e.g.: size of files by file extension).
#' @param ids Vector with tags for which to plot the sizes, one per instance.
#' @param max_ids Total number of top categories (with higher size) to include in the boxplot.
#' @param boxplot_title Main title of the boxplot.
#' @param x_label Label to be used in the x-axis of the plot.
#' @param fill_color Color of the boxes.
#'
#' @export
#'
#' @examples
#' sizes = c(15, 500, 100, 450, 200, 3000, 300, 600)
#' names(sizes) = "MB"
#' ids = c("txt", "txt", "txt", "txt", "bam", "bam", "bam", "gz")
#' plot_boxplot(sizes, ids, 2, "Size in storage", "File Extension")

plot_boxplot = function(sizes, ids, max_ids=20, boxplot_title, x_label, fill_color="steelblue"){

  #..validate inputs..
  if(!is.numeric(sizes)){
    stop("Input argument 'sizes' must be numeric.")
  }
  if(!is.character(ids)){
    stop("Input argument 'ids' must be of type character.")
  }
  if(length(sizes)!=length(ids)){
    stop("Input arguments 'sizes' and 'ids' must have the same length.")
  }


  #..get top values..
  ids_sz = get_sizes(sizes=sizes, tags=ids)#..get size per tag..
  sz_top = get_top(sizes=ids_sz, ids=names(ids_sz), max_ids=max_ids)#..get top data..
  top_filter = ids%in%names(sz_top)#..create filter for positions with the top data..
  sizes_top = sizes[top_filter]#..isolate sizes for top ids..
  names(sizes_top) = colnames(dataset)[1]#..add sz unit..
  ids_top = ids[top_filter]#..isolate top ids..

  #..convert into best unit for plotting..
  sz_unit = names(sizes)[1]
  sizes = convert_unit(sizes_top, in_unit=sz_unit, out_unit="AUTO")
  sz_unit = names(sizes)[1]#..update size unit..

  #..prepare data for ggplot..
  df = data.frame(extension=ids_top, size=sizes)

  #..create plot..
  p = ggplot2::ggplot(data=df, ggplot2::aes(x=extension, y=size)) +
    ggplot2::geom_boxplot(fill=fill_color, colour="black", outlier.shape=NA) +
    ggplot2::geom_jitter(width=0.2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, vjust=1, hjust=1)) +
    ggplot2::ggtitle(boxplot_title) +
    ggplot2::xlab(x_label) + ggplot2::ylab(paste0("Size (", sz_unit, ")"))

  #dev.new()
  plot(p)
}
