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
#'   background fill. Ignored if `custom_fill` is provided.
#' @param fill_alpha Numeric between 0 and 1. Transparency of the fill color.
#' @param border_color Character. Named color for the border. Ignored if
#'   `custom_fill` is provided.
#' @param border_alpha Numeric between 0 and 1. Transparency of the border.
#' @param text_color Character. Named color for the text. Ignored if
#'   `custom_text_color` is provided.
#' @param custom_fill,custom_text_color Character. Optional override colors (hex
#'   or named).
#' @param padding_width,padding_height Numeric. Padding (in inches or relative)
#'   around the text area.
#' @param use_relative_padding Logical. Whether to treat padding as proportion
#'   of device size.
#' @param line_spacing Numeric. Line spacing multiplier.
#' @param font_family Character. Font family (e.g., `"sans"`, `"mono"`,
#'   `"Roboto"`).
#' @param font_face Character. Font face style. One of `"plain"`, `"bold"`,
#'   `"italic"`, or `"bold.italic"`.
#'
#' @return Invisibly returns `NULL`. Produces a graphical output using `grid`
#'   graphics.
#'
#' @importFrom grid textGrob unit grobWidth grobHeight viewport pushViewport
#'   popViewport grid.newpage grid.rect grid.draw editGrob gpar
#' @importFrom scales alpha
#'
#' @seealso [postit_palette], show_postit_palette()]
#'
#' @examples
#' # Simple yellow post-it
#' create_postit("\u270D Post-IT")
#'
#' # With custom colors and bold font
#' create_postit(
#'   text_string = "Analysis Workflow",
#'   fill_color = "blue3",
#'   border_color = "blue3",
#'   text_color = "white",
#'   font_face = "bold",
#'   device_width = 7,
#'   device_height = 6
#' )
#'
#' # With relative padding
#' create_postit(
#'   text_string = "This~is~scalable",
#'   use_relative_padding = TRUE,
#'   padding_width = 0.1,
#'   padding_height = 0.1
#' )
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
    text_color = "black",
    custom_fill = NULL,
    custom_text_color = NULL,
    padding_width = 0.25,
    padding_height = 0.25,
    use_relative_padding = FALSE,
    line_spacing = 1.5,
    font_family = "",
    font_face = "plain"
) {
  # --- Resolve Colors ---
  fill_resolved <- if (!is.null(custom_fill)) {
    custom_fill
  } else {
    resolve_color(fill_color)
  }
  
  border_resolved <- resolve_color(border_color)
  
  text_resolved <- if (!is.null(custom_text_color)) {
    custom_text_color
  } else {
    resolve_color(text_color)
  }
  
  # --- Parse and Wrap Text ---
  parsed <- parse_text(text_string)
  layouts <- wrap_variants(parsed)
  
  # --- Compute Inner Box Size ---
  pad_w <- if (use_relative_padding) {
    device_width * padding_width
  } else {
    padding_width
  }
  
  pad_h <- if (use_relative_padding) {
    device_height * padding_height
  } else {
    padding_height
  }
  
  inner_width  <- device_width - 2 * pad_w
  inner_height <- device_height - 2 * pad_h
  
  # --- Find Best Layout and Font Size ---
  best <- find_best_layout(layouts, inner_width, inner_height, line_spacing)
  
  message("Best font size: ", best$fontsize, " pt")
  message("Text layout:\n", paste(best$layout, collapse = "\n"))
  
  # --- Draw Post-it ---
  draw_background(
    device_width = device_width,
    device_height = device_height,
    fill_color = fill_resolved,
    fill_alpha = fill_alpha,
    border_color = border_resolved,
    border_alpha = border_alpha
  )
  
  draw_text_lines(
    lines = best$layout,
    font_size = best$fontsize,
    color = text_resolved,
    width = inner_width,
    height = inner_height,
    line_spacing = line_spacing,
    font_family = font_family,
    font_face = font_face
  )
  
  invisible(NULL)
}
