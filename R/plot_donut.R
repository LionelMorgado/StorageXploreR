#' @title Donut Chart
#'
#' @description
#' Plot donut for size by category.
#'
#' @param sizes Numeric vector with sizes.
#' @param ids Vector with tags for which to plot the sizes.
#' @param fraction_thresh Threshold for the smaller fractions of the donut to plot.
#' @param donut_title Main title of the donut.
#' @param c_palette Color pallete to be used int the donut segments.
#' @param start_angle Angle to start the plotting.
#'
#' @export
#'
#' @examples
#' sizes = c(10, 500, 60, 3000)
#' names(sizes) = "MB"
#' ids = c("txt", "gz", "pdf", "bam")
#' plot_donut(sizes, ids, 0.4, "Storage used by file extension")

plot_donut = function(sizes, ids, fraction_thresh=0.03, donut_title, c_palette="Set3", start_angle=0){

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
  if(!is.numeric(fraction_thresh)){
    stop("Input argument 'fraction_thresh' must be numeric.")
  }


  #..convert into best unit for plotting..
  sz_unit = names(sizes)[1]
  sizes = convert_unit(sizes, in_unit=sz_unit, out_unit="AUTO")

  #..determine total size
  tot_sz = sum(sizes)
  sz_unit = names(sizes)[1]

  #..determine fractions..
  fractions = sizes/tot_sz

  #..group all entries with less than a user defined % of total storage..
  k_flag = fractions>=fraction_thresh#..determine fractions equal or above the threshold..
  frac_donut = c(fractions[k_flag], sum(fractions[!k_flag]))#..determine fractions of the donut..
  ids_donut = c(ids[k_flag], paste0("Others (<", fraction_thresh*100,"%)"))#..create instance id tags to show in the plot..
  sz_donut = c(sizes[k_flag], sum(sizes[!k_flag]))#..define instance size to show in the plot..

  #..format data to fit ggplot..
  df = data.frame(ids=ids_donut, size=sz_donut)
  df$fraction = frac_donut
  df$ymax = cumsum(df$fraction)
  df$ymin = c(0, head(df$ymax, n=-1))
  df$labelPosition = (df$ymax + df$ymin)/2
  perc_donut = signif(frac_donut*100, digits=3)
  df$label = paste0(ids_donut,"\n", perc_donut, "%")

  #..create plot..
  p = ggplot2::ggplot(df, ggplot2::aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ids)) +
    ggplot2::geom_rect(colour="black") +
    ggrepel::geom_label_repel(ggplot2::aes(x=4, y=labelPosition, label=label),
                     size=4, hjust=0.5,
                     nudge_x=1, direction="x",
                     segment.curvature=-0.1,
                     segment.ncp=3,
                     segment.angle=20, seed=123
    ) +
    ggplot2::coord_polar(theta="y", clip="off", start=start_angle) +
    ggplot2::xlim(c(2, 5)) +
    ggplot2::theme_void() +
    ggplot2::guides(fill="none") +
    ggplot2::scale_fill_brewer(palette=c_palette) +
    ggplot2::ggtitle(donut_title) +
    ggplot2::annotate(geom='text', x=2, y=0, label=paste0("Total:\n", signif(tot_sz, digits=3), sz_unit))#, size=8)

  #dev.new()
  plot(p)
}
