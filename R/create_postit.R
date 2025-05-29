#' Create a stylized ggplot text post
#'
#' Generates a styled plot with text using `ggplot2` and `ggtext::geom_textbox`.
#'
#' @param text The main text string.
#' @param text_color Named color from the paint palette.
#' @param text_size Size of the text in points.
#' @param text_fontface Font face for text (e.g., "plain", "bold").
#' @param text_fontfamily Font family for text.
#' @param fill_color Named color for the background box.
#' @param fill_alpha Opacity for the fill background.
#' @param border_alpha Opacity for the border.
#' @param border_width Width of the border line.
#' @param custom_fill Optional custom hex code to override `fill_color`.
#' @param custom_text_color Optional custom hex code to override `text_color`.
#' @param h_pos Horizontal alignment: one of "left", "center", "right".
#' @param v_pos Vertical alignment: one of "top", "middle", "bottom".
#' @param wrap Whether to wrap text.
#' @param max_line_length Maximum characters per line when wrapping.
#' @param max_lines Maximum number of lines before truncation.
#' @param truncate Whether to truncate lines that exceed `max_lines`.
#' @param auto_scale Whether to auto-reduce font size if text overflows.
#'
#' @return A `ggplot` object with the formatted text.
#'
#' @importFrom ggplot2 ggplot aes coord_cartesian theme_void .pt
#' @importFrom ggtext geom_textbox
#' @importFrom scales alpha
#' @importFrom grid unit
#' @export
#'
#' @examples
#' create_postit("This is an example post.")
create_postit <- function(
    text,
    text_color = "Black",
    text_size = 80,
    text_fontface = "plain",
    text_fontfamily = "sans",
    fill_color = "White",
    fill_alpha = 0.3,
    border_alpha = 1,
    border_width = 10,
    custom_fill = NULL,
    custom_text_color = NULL,
    h_pos = "center",
    v_pos = "middle",
    wrap = TRUE,
    max_line_length = 40,
    max_lines = 5,
    truncate = TRUE,
    auto_scale = TRUE
) {
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
  
  if (wrap) {
    lines <- strwrap(text, width = max_line_length)
    if (truncate && length(lines) > max_lines) {
      lines <- c(lines[1:max_lines], "...")
    }
    text <- paste(lines, collapse = "<br>")
  }
  
  effective_size <- text_size
  if (auto_scale) {
    while (nchar(text) * effective_size * 0.3 > 1000 && effective_size > 5) {
      effective_size <- effective_size - 1
    }
  }
  
  postit <- ggplot(fill="teal1") +
    geom_textbox(
      aes(x = x_pos, y = y_pos, label = text),
      fill = alpha(fill_color, fill_alpha),
      color = text_color,
      size = effective_size / .pt,
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
  print(postit)
  return(postit)
}
