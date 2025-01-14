#' @title Boxplot
#'
#' @description
#' Boxplot for sizes for instances grouped by category.
#'
#' @param sizes Numeric vector with sizes.
#' @param tags String vector with category tags for which to plot the sizes, one per instance.
#' @param max_tags Total number of top categories (with higher size) to include in the boxplot.
#' @param boxplot_title Main title of the boxplot.
#' @param x_label Label to be used in the x-axis of the plot.
#' @param fill_color Color of the boxes.
#'
#' @export
#'
#' @examples
#' sizes = c(15, 500, 100, 450, 200, 3000, 300, 600)
#' names(sizes) = "MB"
#' tags = c("txt", "txt", "txt", "txt", "bam", "bam", "bam", "gz")
#' plot_boxplot(sizes, tags, 2, "Size in storage", "File Extension")

plot_boxplot = function(sizes, tags, max_tags=20, boxplot_title, x_label, fill_color="steelblue"){

  #..validate inputs..
  if(!is.numeric(sizes)){
    stop("Input argument 'sizes' must be numeric.")
  }
  if(!is.character(tags)){
    stop("Input argument 'tags' must be of type character.")
  }
  if(length(sizes)!=length(tags)){
    stop("Input arguments 'sizes' and 'tags' must have the same length.")
  }


  #..get top values..
  tags_sz = get_sizes(sizes=sizes, tags=tags)#..get size per tag..
  sz_top = get_top(sizes=tags_sz, tags=names(tags_sz), max_tags=max_tags)#..get top data..
  top_filter = tags%in%names(sz_top)#..create filter for positions with the top data..
  sizes_top = sizes[top_filter]#..isolate sizes for top tags..
  names(sizes_top) = colnames(dataset)[1]#..add sz unit..
  tags_top = tags[top_filter]#..isolate top tags..

  #..convert into best unit for plotting..
  sz_unit = names(sizes)[1]
  sizes = convert_unit(sizes_top, in_unit=sz_unit, out_unit="AUTO")
  sz_unit = names(sizes)[1]#..update size unit..

  #..prepare data for ggplot..
  df = data.frame(extension=tags_top, size=sizes)

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
