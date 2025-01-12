#' @title Barplot
#'
#' @description
#' Barplot for sizes by category.
#'
#' @param sizes Numeric vector with category sizes.
#' @param tags Vector with tags for which to plot the sizes.
#' @param barplot_title Main title of the barplot.
#' @param max_tags Maximum tags for which to plot.
#' @param x_label Label to be used in the x-axis of the plot.
#' @param fill_color Color of the bars.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bar
#'
#' @examples
#' sizes = c(10, 500, 250, 3000)
#' names(sizes) = "MB"
#' tags = c("txt", "gz", "pdf", "bam")
#' plot_barplot(sizes, tags, 3, "Size in storage", "File Extension")

plot_barplot = function(sizes, tags, max_tags=20, barplot_title, x_label="Instance", fill_color="steelblue"){

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
  if(!is.numeric(max_tags)){
    stop("Input argument 'max_tags' must be numeric.")
  }


  #..get top values..
  sz_top = get_top(sizes=sizes, tags=tags, max_tags=max_tags)

  #..convert into best unit for plotting..
  sz_unit = names(sizes)[1]
  sizes = convert_unit(sz_top, in_unit=sz_unit, out_unit="AUTO")

  #..prepare data for ggplot..
  df = data.frame(id=names(sz_top), size=sizes)

  #..create plot..
  p = ggplot2::ggplot(data=df, ggplot2::aes(x=id, y=size)) +
    ggplot2::geom_bar(stat="identity", fill=fill_color, colour="black") +
    ggplot2::scale_x_discrete(limits=df$id) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, vjust=1, hjust=1)) +
    ggplot2::ggtitle(barplot_title) +
    ggplot2::xlab(x_label) + ggplot2::ylab(paste0("Size (", names(sizes), ")"))

  #dev.new()
  plot(p)
}
