#' Create a Post-it Style Graphic with Text
#'
#' Generates a Post-it note graphic with wrapped and centered text. The function
#' scales font size to fit within a specified area, applying configurable
#' padding, color themes, and typography.
#'
#' @param text_string Character. The text string to display. Use `~` to glue
#'   words and `//` to separate blocks.
#' @param device_width,device_height Numeric. Width and height in inches of the
#'   output device.
#' @param fill_color Character. Named color (from `postit_palette`) for
#'   background fill. Ignored if `custom_fill_color` is provided.
#' @param fill_alpha Numeric between 0 and 1. Transparency of the fill color.
#' @param border_color Character. Named color for the border. Ignored if
#'   `custom_border_color` is provided.
#' @param border_alpha Numeric between 0 and 1. Transparency of the border.
#' @param border_size Numeric. Width of the border line in points. Use larger
#'   values for thicker borders. Defaults to 1.
#' @param text_color Character. Named color for the text. Ignored if
#'   `custom_text_color` is provided.
#' @param custom_fill_color,custom_border_color,custom_text_color Character.
#'   Optional override colors (hex or R color names) that bypass the post-it
#'   palette.
#' @param padding_width,padding_height Numeric. Padding (in inches or relative)
#'   around the text area.
#' @param use_relative_padding Logical. Whether to treat padding as proportion
#'   of device size.
#' @param line_spacing Numeric. Line spacing multiplier.
#' @param font_family Character. Font family (e.g., `"sans"`, `"mono"`,
#'   `"Roboto"`).
#' @param font_face Character. Font face style. One of `"plain"`, `"bold"`,
#'   `"italic"`, or `"bold.italic"`.
#' @param output Character. Determines how the post-it graphic is handled. Use
#'   `"plot"` (default) to render the post-it directly using grid graphics. Use
#'   `"object"` to return a patchwork-compatible wrapped grob object (without
#'   drawing), suitable for composition with other ggplot2 or grid-based plots.
#' @param rstudio Logical. If `TRUE`, attempts to close an open graphics device
#'   in RStudio to avoid overlapping or unintended plot rendering. This is
#'   mainly useful when using `output = "object"` in `create_postit()` inside
#'   RStudio, where the default device may cause display artifacts or layering
#'   issues. This issue does not appear on NIDAP.
#'
#' @return If `output = "plot"`, the function invisibly returns `NULL` and draws
#'   the post-it note to the current graphics device. If `output = "object"`, a
#'   patchwork-compatible ggplot object (containing a grid grob) is returned and
#'   nothing is drawn.
#'
#' @importFrom grid textGrob unit grobWidth grobHeight viewport pushViewport
#'   popViewport grid.newpage grid.rect grid.draw editGrob gpar
#' @importFrom patchwork wrap_elements
#' @importFrom scales alpha
#'
#' @seealso [postit_palette()], [show_postit_palette()]
#'
#' @export
create_postit <- function(
    text_string,
    device_width = 12,
    device_height = 7,
    fill_color = "yellow1",
    fill_alpha = 0.3,
    border_color = "yellow3",
    border_alpha = 1,
    border_size = 1,
    text_color = "black",
    custom_fill_color = NULL,
    custom_border_color = NULL,
    custom_text_color = NULL,
    padding_width = 0.1,
    padding_height = 0.1,
    use_relative_padding = TRUE,
    line_spacing = 1.5,
    font_family = "",
    font_face = "plain",
    output = c("plot", "object"),
    rstudio = FALSE) {
 
   output <- match.arg(output)
  
  # Resolve colors
  fill_resolved <- if (!is.null(custom_fill_color))
    custom_fill_color
  else
    resolve_color(fill_color)
  border_resolved <- if (!is.null(custom_border_color))
    custom_border_color
  else
    resolve_color(border_color)
  text_resolved <- if (!is.null(custom_text_color))
    custom_text_color
  else
    resolve_color(text_color)
  
  # Parse and layout
  parsed <- parse_text(text_string)
  layouts <- wrap_variants(parsed)
  
  pad_w <- if (use_relative_padding)
    device_width * padding_width
  else
    padding_width
  pad_h <- if (use_relative_padding)
    device_height * padding_height
  else
    padding_height
  inner_width  <- device_width - 2 * pad_w
  inner_height <- device_height - 2 * pad_h
  
  best <- find_best_layout(layouts, inner_width, inner_height, line_spacing)
  
  message("Best font size: ", best$fontsize, " pt")
  message("Text layout:\n", paste(best$layout, collapse = "\n"))
  
  # Construct grobs
  outer_vp <- grid::viewport(
    width = grid::unit(device_width, "inches"),
    height = grid::unit(device_height, "inches")
  )
  
  # Background rectangle
  bg_grob <- grid::rectGrob(
    x = 0.5,
    y = 0.5,
    width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"),
    gp = grid::gpar(
      fill = scales::alpha(fill_resolved, fill_alpha),
      col  = scales::alpha(border_resolved, border_alpha),
      lwd  = border_size
    )
  )
  
  # Text grobs
  grobs <- lapply(best$layout, function(line) {
    grid::textGrob(
      line,
      gp = grid::gpar(
        fontsize = best$fontsize,
        col = text_resolved,
        fontfamily = font_family,
        fontface = font_face
      )
    )
  })
  
  heights <- sapply(grobs, function(g) {
    grid::convertHeight(grid::grobHeight(g), "npc", valueOnly = TRUE)
  })
  
  if (length(grobs) > 1) {
    spaced_heights <- heights * line_spacing
  } else {
    spaced_heights <- heights
  }
  
  total_height <- sum(spaced_heights)
  top_offset <- if (length(grobs) > 1) {
    (line_spacing - 1) * mean(heights) / 2
  } else {
      0
  }
  line_offsets <- cumsum(c(0, head(spaced_heights, -1)))
  y_positions <- 0.5 + (total_height / 2) - line_offsets - top_offset
  
  text_grobs <- mapply(function(g, y) {
    grid::editGrob(g, y = grid::unit(y, "npc"), just = "top")
  }, grobs, y_positions, SIMPLIFY = FALSE)
  
  composed <- grid::grobTree(
    bg_grob,
    grobs = do.call(grid::gList, text_grobs),
   vp = outer_vp)
  
  if (output == "object") {
    if(rstudio) {
      if (dev.cur() > 1) grDevices::dev.off()
    }
    return(invisible(patchwork::wrap_elements(composed)))
  } else {
    grid.newpage()
    pushViewport(outer_vp)
    grid.draw(composed)
    popViewport()
    invisible(NULL)
  }
}
