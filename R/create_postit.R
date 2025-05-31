#' Create a styled text label as a ggplot object
#'
#' Returns a ggplot object that displays styled text using
#' `ggtext::geom_textbox`. Intended for labeling nodes within NIDAP Code
#' Workbooks, such as marking workflow stages, annotating data entry points, or
#' visually segmenting sections in multi-step pipelines. The function allows
#' customization of text content, font styling, colors, positioning, wrapping,
#' abbreviation, truncation, and automatic adjustment based on the text area.
#'
#' @param text Character string. The main message to display.
#' @param text_color Named color from the internal paint palette for the text.
#' @param text_size Numeric. Font size in points.
#' @param text_fontface Font face (e.g., "plain", "bold", "italic").
#' @param text_fontfamily Font family (e.g., "sans", "serif").
#' @param fill_color Named background color from the paint palette.
#' @param fill_alpha Opacity of the background fill (0 to 1).
#' @param border_alpha Opacity of the border color (0 to 1).
#' @param border_width Numeric. Width of the border line.
#' @param custom_fill Optional custom hex color code to override `fill_color`.
#' @param custom_text_color Optional hex color code to override `text_color`.
#' @param h_pos Horizontal alignment of the text: "left", "center", or "right".
#' @param v_pos Vertical alignment of the text: "top", "middle", or "bottom".
#' @param wrap Logical. Whether to wrap long text lines.
#' @param truncate Logical. Whether to truncate overflow text.
#' @param abbreviate_text Logical. Whether to abbreviate individual words.
#' @param abbr_minlength Integer. Minimum abbreviation length.
#' @param plot Logical. Whether to display the plot (TRUE) or only return the
#'   ggplot object (FALSE).
#' @return A `ggplot` object containing a stylized textbox with the specified
#'   text.
#'
#' @importFrom ggplot2 ggplot aes coord_cartesian theme_void .pt
#' @importFrom ggtext geom_textbox
#' @importFrom scales alpha
#' @importFrom grid unit
#' @export
#'
#' @examples
#' create_postit("Step 2: DEG Analysis", fill_color = "blue1")
create_postit <- function(text = "Post-IT",
                          text_color = "Black",
                          text_size = 200,
                          text_fontface = "plain",
                          text_fontfamily = "sans",
                          fill_color = "yellow1",
                          fill_alpha = 0.3,
                          border_alpha = 1,
                          border_width = 10,
                          custom_fill = NULL,
                          custom_text_color = NULL,
                          h_pos = "center",
                          v_pos = "middle",
                          wrap = TRUE,
                          truncate = TRUE,
                          abbreviate_text = FALSE,
                          abbr_minlength = NULL,
                          plot = TRUE) {
  
  paint_palette <- c(
    darkgray1 = "#4D4D4D", darkgray2 = "#333333", black = "#000000",
    gray1 = "#999999", gray2 = "#808080", gray3 = "#666666",
    white = "#FFFFFF", lightgray1 = "#CCCCCC", lightgray2 = "#B3B3B3",
    red1 = "#F44E3B", red2 = "#D33115", red3 = "#9F0500",
    orange1 = "#FE9200", orange2 = "#E27300", orange3 = "#C45100",
    yellow1 = "#FCDC00", yellow2 = "#FCC400", yellow3 = "#FB9E00",
    yellowgreen1 = "#DBDF00", yellowgreen2 = "#B0BC00", yellowgreen3 = "#808900",
    green1 = "#A4DD00", green2 = "#68BC00", green3 = "#194D33",
    teal1 = "#68CCCA", teal2 = "#16A5A5", teal3 = "#0C797D",
    blue1 = "#73D8FF", blue2 = "#009CE0", blue3 = "#0062B1",
    purple1 = "#AEA1FF", purple2 = "#7B64FF", purple3 = "#653294",
    magenta1 = "#FDA1FF", magenta2 = "#FA28FF", magenta3 = "#AB149E"
  )
  
  fill_color <- if (!is.null(custom_fill)) custom_fill else paint_palette[[tolower(fill_color)]]
  text_color <- if (!is.null(custom_text_color)) custom_text_color else paint_palette[[tolower(text_color)]]
  border_color <- fill_color
  
  hjust <- switch(h_pos, left = 0, center = 0.5, right = 1, 0.5)
  vjust <- switch(v_pos, bottom = 0, middle = 0.5, top = 1, 0.5)
  
  x_pos <- hjust
  y_pos <- vjust
  
  processed_text <- process_postit_text(
    text = text,
    width_npc = 1,
    height_npc = 1,
    text_size = text_size,
    wrap = wrap,
    truncate = truncate,
    abbreviate_text = abbreviate_text,
    abbr_minlength = abbr_minlength,
    font_family = text_fontfamily,
    font_face = text_fontface
  )
  
  postit <- ggplot() +
    geom_textbox(
      aes(x = x_pos, y = y_pos, label = processed_text),
      fill = alpha(fill_color, fill_alpha),
      color = text_color,
      size = text_size / .pt,
      family = text_fontfamily,
      fontface = text_fontface,
      halign = hjust,
      valign = vjust,
      box.colour = alpha(border_color, border_alpha),
      box.size = border_width,
      width = unit(1, "npc"),
      height = unit(1, "npc")
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void()
  
  if (plot) print(postit)
  return(postit)
}
