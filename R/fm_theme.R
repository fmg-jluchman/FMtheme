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
#' _FMtheme_ are intended to work with the _linetype_ aesthetic for this geom.
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
#' dot graphs.  This includes labels on data points in the graphic.
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

#' @title Informal Fors Marsh ggplot2 theme
#' @name theme_fmlite
#' @description
#' A `ggplot` theme that uses an informal Fors Marsh style.
#'
#' @param fontpath Path to Berlingske Sans fonts.
#'
#' @param color Character name of a Fors Marsh brand color.
#'
#' @param ... Passes arguments to other methods.
#'
#' @details
#' Many analysts might be looking for a way to incorporate a Fors Marsh style
#' for less formal deliverables that do not need to be 508 compliant and need
#' to be easier to work with in markdown documentation where graph sizing
#' can vary.
#'
#' The focus of `theme_fmlite` is to provide an easy mechanism for analysts to
#' apply some of the most important elements of the Fors Marsh visual
#' signature to graphics without many of the constraints imposed by `theme_fm`
#' and its related scales and geoms.
#'
#' `theme_fmlite` is not recommended for formal deliverables without a review
#' by the Creative Craft Center but can, and should, be applied to
#' informal graphics like markdowns that may, or may not, be submitted
#' to clients as documentation.
#'
#' ## Fors Marsh Theme "Lite"
#'
#' The "lite" theme focuses on text and incorporates the
#' *Berlingske Sans Medium* font for all text without restrictions on font size.
#' The plot title for the theme will use *Berlingske Sans Poster*.
#'
#' Most other aspects of the "lite" theme inherit from [`theme_bw`] save that
#' the plot grid is removed to make the background very minimal.
#'
#' Note that the "lite" theme allows for `strip`s (i.e., graph faceting) and
#' `title`s.
#'
#' ## Fors Marsh Theme Colors and Fills
#'
#' The "lite" theme also includes two Fors Marsh color scales:
#' `scale_color_fmlite` and `scale_fill_fmlite`. In addition, the "lite" theme
#' includes a lookup function, `color_finder_fmlite` that returns color hex
#' values given a color name. There are eight total colors and all are named
#' according to the Fors Marsh brand guide. These names include:
#'
#' * Fors Marsh Orange
#' * Pastel Pink
#' * Aquamarine
#' * Blue
#' * Deep Orange
#' * Tangerine
#' * Bright Green
#' * Violet
#'
#' Note that `scale_color_fmlite` and `scale_fill_fmlite` assume you want to
#' use all eight Fors Marsh brand colors, sequentially, from "Fors Marsh Orange"
#' to "Violet". Only these eight values work and, beyond eight levels for an
#' aesthetic, these scales will return nothing and suppress the geoms to which
#' they are supposed to apply.
#'
#' As an alternative to `scale_color_fmlite` and `scale_fill_fmlite`,
#' you can use `scale_color_manual` or `scale_fill_manual` and plug in
#' different values using `color_finder_fmlite`. For example, a user could
#' construct a sequence of values using
#' `color_finder_fmlite("Fors Marsh Orange")` and
#' `color_finder_fmlite("Violet")` which return the hex color value
#' "#FFA219" corresponding with "Fors Marsh Orange" and "#8353FF" corresponding
#' with "Violet" for contrasting colors in the graph.
#'
#' @section Key Considerations:
#'
#' There are no specific considerations for the use of `theme_fmlite`. You do
#' not need to use `ggsave_fm` to save graphs with the "lite" theme. You can
#' use `facet_grid` or `facet_wrap` with the "lite" theme. You can also use
#' as many aesthetics as you would like with the "lite" theme. In addition,
#' you can add titles and captions to the graphics as needed with the "lite"
#' theme.
#'
#' @examples
#' require(ggplot2)
#' require(datasets)
#'
#' state_data <-
#'   data.frame(region = state.region, state.x77) |>
#'   transform(
#'     Population =
#'       cut(Population,
#'         quantile(Population, probs = c(0, .5, 1)), c("Low", "High"),
#'         include.lowest = TRUE
#'       ),
#'     Income =
#'       cut(Income,
#'         quantile(Income, probs = c(0, .5, 1)), c("Low", "High"),
#'         include.lowest = TRUE
#'       )
#'   )
#'
#' ## Example Bar Chart
#' state_data |>
#'   aggregate(Illiteracy ~ region + Population, mean, data = _) |>
#'   ggplot(aes(region, Illiteracy, fill = Population)) + scale_fill_fmlite() +
#'   geom_col(position = position_dodge()) + theme_fmlite() +
#'   ggtitle("Illiteracy in 1977")
#'
#' ## Example Line Plot
#' state_data |>
#'   ggplot(aes(Illiteracy, HS.Grad, linetype = region, group = region)) +
#'   scale_linetype_discrete() + theme_fmlite() + scale_linetype_discrete() +
#'   geom_smooth(method = "lm", se = FALSE,
#'     color = color_finder_fmlite("Fors Marsh Orange"))
#'
#' ## Example Dot Plot
#' state_data |>
#'   ggplot(aes(Illiteracy, HS.Grad, color = region)) +
#'   geom_point() + theme_fmlite() +
#'   scale_color_fmlite() +
#'   facet_grid(cols = vars(Population)) +
#'   labs(caption = "By Population Level")
#'
#' @import ggplot2
#' @export
theme_fmlite <- function(fontpath = "C:/Windows/Fonts/", ...) {
    sysfonts::font_add("Berlingske Sans Medium",
                       paste0(fontpath, "BerlingskeSans-Md.otf"))
    sysfonts::font_add("Berlingske Sans Poster",
                       paste0(fontpath, "BerlingskeSans-Pst.otf"))
    sysfonts::font_add("Berlingske Serif Condensed Medium",
                       paste0(fontpath, "BerlingskeSerifCn-Md.otf"))
    showtext::showtext_auto()
    temp_theme <-
      theme_bw() +
      theme(
        text = element_text(family = "Berlingske Sans Medium"),
        plot.title = element_text(family = "Berlingske Sans Poster"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
      )
    temp_theme
}

#' @rdname theme_fmlite
#' @export
scale_fill_fmlite <-
  function(...) {
    pal <-
      function(n) {
        c(
          grDevices::rgb(255, 162,  25, 255, maxColorValue = 255),
          grDevices::rgb(254, 204, 221, 255, maxColorValue = 255),
          grDevices::rgb(107, 225, 202, 255, maxColorValue = 255),
          grDevices::rgb(124, 220, 255, 255, maxColorValue = 255),
          grDevices::rgb(204,  86,  38, 255, maxColorValue = 255),
          grDevices::rgb(255, 121,  31, 255, maxColorValue = 255),
          grDevices::rgb(127, 233,  72, 255, maxColorValue = 255),
          grDevices::rgb(131,  83, 255, 255, maxColorValue = 255)
          )[seq_len(n)]
      }
    discrete_scale(aesthetics = "fill", palette = pal, ...)
  }

#' @rdname theme_fmlite
#' @export
scale_color_fmlite <-
  function(...) {
    pal <-
      function(n) {
        c(
          grDevices::rgb(255, 162,  25, 255, maxColorValue = 255),
          grDevices::rgb(254, 204, 221, 255, maxColorValue = 255),
          grDevices::rgb(107, 225, 202, 255, maxColorValue = 255),
          grDevices::rgb(124, 220, 255, 255, maxColorValue = 255),
          grDevices::rgb(204,  86,  38, 255, maxColorValue = 255),
          grDevices::rgb(255, 121,  31, 255, maxColorValue = 255),
          grDevices::rgb(127, 233,  72, 255, maxColorValue = 255),
          grDevices::rgb(131,  83, 255, 255, maxColorValue = 255)
        )[seq_len(n)]
      }
    discrete_scale(aesthetics = "color", palette = pal, ...)
  }

#' @rdname theme_fmlite
#' @export
color_finder_fmlite <-
  function(color) {
    switch(
      color,
      "Fors Marsh Orange" = "#FFA219",
      "Pastel Pink" =       "#FECCDD",
      "Aquamarine" =        "#68E1CA",
      "Blue" =              "#7CDCFF",
      "Deep Orange" =       "#CC5626",
      "Tangerine" =         "#FF791F",
      "Bright Green" =      "#7FE948",
      "Violet" =            "#8353FF"
    )
  }
