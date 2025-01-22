#' @title Fors Marsh ggplot2 theme
#' @name theme_fm
#' @description
#' A `ggplot` theme that uses Fors Marsh style guidelines.
#'
#' @param fontpath Path to Berlingske Sans fonts.
#'
#' @param ... Passes arguments to other methods.
#'
#' @details
#' Fors Marsh's brand guidelines use fonts, color, linestyles, and symbols as
#' distinctive visual markers of the Seriously Human Fors Marsh brand in
#' graphics depicting data analysis results.
#'
#' _FMtheme_ includes the `theme_fm` that applies the background style for all
#' graphics and fonts. Aesthetics such as colors, shapes and linetypes are
#' implemented in separate functions to be used as `scale` functions.
#' These scale functions, and several implementations of `geom`s, are discussed
#' in separate sections below.
#'
#' Thematic elements that currently do not have a specific implementation in
#' Fors Marsh theme inheriting from [`ggplot2::theme_bw`].
#' The _FMtheme_ package supports most line-, bar-, and dot-based graphics.
#' Our recommendations on how to implement each is discussed below.
#'
#' ## Line Graphs
#'
#' Line-based graphics tend to use [`ggplot2::geom_line`] and the functions in
#' _FMtheme_ are intedned to work with the _linetype_ aesthetic for this geom.
#'
#' We recommend keeping all lines black in color and using `scale_linetype_fm`
#' to differentiate different trendlines when used as an aesthetic in the
#' same plot. `scale_linetype_fm` allows up to five different line patterns in
#' the same plot.
#'
#' ## Bar Graphs
#'
#' Bar-based graphics tend to use [`ggplot2::geom_col`] and the functions in
#' _FMtheme_ are intended to work with both the _linetype_ and _fill_
#' aesthetics for these geoms.
#'
#' We recommend using a combination of `scale_linebar_fm` and `scale_fillbar_fm`
#' to give aesthetics to both the _linetype_ and _fill_ when
#' differentiating between trends in the same plot.
#' `scale_linebar_fm` and `scale_fillbar_fm` allow up to five different line
#' patterns and bar colors in the same plot. We also recommend using
#' `geom_col_fm` when creating a bar graph to give the _linetype_ and
#' _color_ aesthetics the correct defaults.
#'
#' It is important to always use both `scale_linebar_fm` and `scale_fillbar_fm`
#' when constructing bar graphs as, when used alone, these aesthetics can fail
#' to be 508 compliant as the _fill_ colors are not designed to be
#' distinguishable. We also recommend using [`ggplot2::position_dodge`]
#' when using multiple trends as opposed to other position methods.
#'
#' When plotting a histogram or other similar single trend, consider using
#' only black _fill_ and black _color_ bars with solid _linetype_ instead of
#' these scales. These scales are mostly useful for depicting multiple trends.
#'
#' ## Dot Graphs
#'
#' Dot-based graphics use [`ggplot2::geom_point`] and the functions in _FMtheme_
#' are intended to work with both the _shape_ and _fill_ aesthetics for this
#' geom.
#'
#' We recommend using a combination of `scale_shape_fm` and `scale_fillshape_fm`
#' to give aesthetics to both the _shape_ and _fill_ when
#' differentiating between trends in the same plot.
#' `scale_shape_fm` and `scale_fillshape_fm` allow up to five different shapes
#' and shape colors in the same plot. We also recommend using
#' `geom_shape_fm` when creating a bar graph to give the _color_ and _size_
#' aesthetics the correct defaults.
#'
#' It is important to always use both `scale_shape_fm` and `scale_fillsjape_fm`
#' when constructing dot graphs as, when used alone, these aesthetics can fail
#' to be 508 compliant as the _fill_ colors are not designed to be
#' distinguishable.
#'
#' When plotting a scatterplot or other similar single trend, consider using
#' only black _fill_ and number 16/filled in circle _shape_ points with
#' black _color_ instead of these scales. These scales are mostly useful for
#' depicting multiple trends.
#'
#' @section Key Considerations:
#'
#' There are a number of strong suggestions for how to use the functions in
#' _FMtheme_ when generating graphs. If you want to use any of the below these
#' graph features or change defaults to the theme, scales, or geoms defined
#' in the provided functions, first consult the Creative Craft Center.
#'
#' These suggestions are binned together in conceptual categories below.
#'
#' ## Don't use `facet_grid` or `facet_wrap`
#'
#' Broadly, do not use `facet_grid` or `facet_wrap`. The use of faceting
#' affects the proportionality of the graphs and can affect 508 compliance in
#' unexpected ways.
#'
#' As opposed to generating graphs using faceting, create separate graphs
#' by the variables on which you were intending to facet and combine them as
#' separate graphics in the final document in a way that can facilitate
#' comparing them.
#'
#' ## Don't add more aesthetics
#'
#' Do not add colors, symbols, or other aesthetics to the lines, bars, or
#' dot graps.  This includes labels on data points in the graphic.
#' The design of each graph has been customized to be branded, 508 compliant,
#' easy to read for clients and stakeholders, and easy to use for R users with
#' _ggplot2_ familiarity.
#'
#' Changing visual design aspects, adding text to the graph region, and other
#' elements of the graph may make the graph fail to be branded, 508 compliant,
#' or easy to read.
#'
#' ## Don't add titles or captions
#'
#' Do not add graph titles, subtitles, captions, and other notes that are added
#' to graphs. Any such title or note could be added to the document in which
#' the graphic is to be included and is probably a lot easier to edit in the
#' document than as embedded in the graph.
#'
#' Axis titles and legend entries do not apply to this recommendation and
#' clear, concise labels for aesthetics and axes can be useful for readers.
#'
#'## Save with `ggsave_fm`
#'
#' [`ggsave_fm`] is designed to retain the proper proportionality for any
#' graphic produced. Using [`ggplot2::ggsave`] does not respect the graph's
#' intended proportionality and may make it 508 uncompliant as a result.
#'
#' @examples
#' require(ggplot2)
#' require(datasets)
#'
#' ## Example Bar Chart
#' mtcars |>
#'   aggregate(mpg ~ vs + am, mean, data = _) |>
#'   transform(am = as.factor(am), vs = as.factor(vs)) |>
#'   ggplot(aes(vs, mpg, linetype = am, fill = am))  +
#'   scale_linebar_fm() + scale_fillbar_fm() +
#'   geom_col_fm(position = position_dodge()) + theme_fm()
#'
#' ## Example Line Plot
#' mtcars |>
#'   aggregate(mpg ~ gear + cyl, mean, data = _) |>
#'   transform(gear = as.factor(gear))|>
#'   ggplot(aes(cyl, mpg, linetype = gear, group = gear)) +
#'   scale_linetype_fm() + geom_line() + theme_fm()
#'
#' ## Example Dot Plot
#' mtcars |>
#'   transform(gear = as.factor(gear)) |>
#'   ggplot(aes(qsec, mpg, shape = gear, fill = gear)) +
#'   geom_point_fm() + theme_fm() +
#'   scale_fillshape_fm() + scale_shape_fm()
#'
#' @import ggplot2
#' @export
theme_fm <-
  function(fontpath = "C:/Windows/Fonts/", ...) {
    sysfonts::font_add("Berlingske Sans Medium",
                       paste0(fontpath, "BerlingskeSans-Md.otf"))
    sysfonts::font_add("Berlingske Sans Poster",
                       paste0(fontpath, "BerlingskeSans-Pst.otf"))
    showtext::showtext_auto()
    temp_theme <-
      theme_bw() +
      theme(
        # overall lines
        line = element_line(color = "black", linetype = "solid",
                            linewidth = 2 * 0.352), # pt is .352 mm
        # overall rectangular spaces
        rect = element_rect(fill = "white", color = "white"),
        # titles and notes
        text = element_text(family = "Berlingske Sans Medium", size = 12),
        title = element_blank(),
        axis.title = element_text(family = "Berlingske Sans Medium", size = 12),
        # axes
        axis.text = element_text(family = "Berlingske Sans Medium", size = 12),
        axis.text.y = element_text(angle = 90, hjust = .5),
        axis.ticks.length = unit(2, "pt"),
        axis.minor.ticks.length = unit(0, "pt"),
        axis.line = element_line(color = "black",
                                 linetype = "solid", linewidth = 2 * 0.352),
        # legends
        legend.position = "bottom",
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing = unit(2, "pt"),
        legend.frame = element_rect(fill = "white", color = "white"),
        legend.title = element_text(family = "Berlingske Sans Medium",
                                   size = 12),
        legend.byrow = TRUE,
        legend.key.height = unit(2, "pt"),
        legend.key.width = unit(20, "pt"),
        legend.text = element_text(family = "Berlingske Sans Medium",
                                  size = 12),
        # panel
        panel.border = element_blank(),
        panel.grid = element_blank(),
        # plot
        plot.margin = margin(10, 10, 10, 10),
        strip.background = element_blank(),
        strip.text = element_blank()
      )
    temp_theme
  }

#' @rdname theme_fm
#' @export
scale_linetype_fm <-
  function(...) {
    pal <-
      function(n) {
        if (n > 5)
          stop("'scale_linetype_fm()' doesn't allow more than 5 levels.",
               call. = FALSE)
        c("solid", "62", "12", "1262", "121242")[seq_len(n)]
      }
    discrete_scale(aesthetics = "linetype", palette = pal,
                   guide = guide_legend(ncol = 2), ...)
  }

#' @rdname theme_fm
#' @export
scale_shape_fm <-
  function(...) {
    pal <-
      function(n) {
        if (n > 5) stop("'scale_shape_fm()' doesn't allow more than 5 levels.",
                        call. = FALSE)
        c(21, 3, 22, 17, 23)[seq_len(n)]
      }
    discrete_scale(aesthetics = "shape", palette = pal,
                   guide = guide_legend(ncol = 2), ...)
  }

#' @rdname theme_fm
#' @export
scale_fillshape_fm <-
  function(...) {
    pal <-
      function(n) {
        if (n > 5)
          stop("'scale_fillshape_fm()' doesn't allow more than 5 levels.",
               call. = FALSE)
        c(
          grDevices::rgb(255, 162,  25, 255, maxColorValue = 255),
          "black",
          grDevices::rgb(254, 204, 221, 255, maxColorValue = 255),
          "black",
          grDevices::rgb(124, 220, 255, 255, maxColorValue = 255))[seq_len(n)]
      }
    discrete_scale(aesthetics = "fill", palette = pal,
                   guide = guide_legend(ncol = 2), ...)
  }

#' @rdname theme_fm
#' @export
scale_fillbar_fm <-
  function(...) {
    pal <-
      function(n) {
        if (n > 5)
          stop("'scale_fillbar_fm()' doesn't allow more than 5 levels.",
               call. = FALSE)
        c(
          grDevices::rgb(255, 162,  25, 255, maxColorValue = 255),
          grDevices::rgb(254, 204, 221, 255, maxColorValue = 255),
          grDevices::rgb(107, 225, 202, 255, maxColorValue = 255),
          grDevices::rgb(124, 220, 255, 255, maxColorValue = 255),
          grDevices::rgb(131,  83, 255, 255, maxColorValue = 255))[seq_len(n)]
      }
    discrete_scale(aesthetics = "fill", palette = pal,
                   guide = guide_legend(ncol = 2), ...)
  }

#' @rdname theme_fm
#' @export
scale_linebar_fm <-
  function(...) {
    pal <-
      function(n) {
        if (n > 5)
          stop("'scale_linebar_fm()' doesn't allow more than 5 levels.",
               call. = FALSE)
        c("solid", "dashed", "11", "ac", "1222")[seq_len(n)]
      }
    discrete_scale(aesthetics = "linetype", palette = pal,
                   guide = guide_legend(ncol = 2), ...)
  }


#' @rdname theme_fm
#' @export
geom_col_fm <-
  function(...) {
    geom_col(..., color = "black", linewidth = .5)
  }

#' @rdname theme_fm
#' @export
geom_point_fm <-
  function(...) {
    geom_point(..., size = 2)
  }

#' @title Fors Marsh themed plot saving
#' @name ggsave_fm
#' @description
#' Saving method appropriate for Fors Marsh themed graphics.
#'
#' @param filename 	File name to create on disk.
#'
#' @param ... Other arguments passed on to `ggsave` and the graphics device
#' function, as specified by device.
#'
#' @details
#'
#' Note that the `width` and `height` arguments cannot be passed to `ggsave`.
#' These values are fixed by `ggsave_fm` and will produce an error if
#' specified by the user.
#'
#' @export
ggsave_fm <-
  function(filename, ...) {
    ggsave(filename = filename, width = 5, height = 4, ...)
  }
