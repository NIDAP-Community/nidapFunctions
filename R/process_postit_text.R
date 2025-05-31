#' Internal helper to process and format text for create_postit()
#'
#' Automatically computes how much text can fit inside the ggplot textbox based
#' on text size and available space (npc units). It applies abbreviation,
#' wrapping, and truncation as needed. Supports dynamic adjustment for font size
#' and plot dimensions.
#'
#' @param text The input character string.
#' @param width_npc Width available for text (in npc units).
#' @param height_npc Height available for text (in npc units).
#' @param text_size Font size in points.
#' @param wrap Logical. Whether to wrap text into multiple lines.
#' @param truncate Logical. Whether to truncate text if it overflows.
#' @param abbreviate_text Logical. If TRUE, applies abbreviation.
#' @param abbr_minlength Integer or NULL. Minimum abbreviation length (defaults
#'   to dynamic).
#' @param font_family Character. Font family used for text (affects average char
#'   width).
#' @param font_face Character. Font face ("plain", "italic", etc.).
#'
#' @return A processed character string suitable for rendering in a ggplot
#'   label.
#' @keywords internal
process_postit_text <- function(
    text,
    width_npc = 1,
    height_npc = 1,
    text_size = 200,
    wrap = TRUE,
    truncate = TRUE,
    abbreviate_text = FALSE,
    abbr_minlength = NULL,
    font_family = "sans",
    font_face = "plain"
) {
  # Estimate available space in characters and lines
  pt_to_npc <- 1 / 72 / 12  # approximate scale: 1 pt = ~1/864 npc
  char_width_npc <- pt_to_npc * text_size * 0.6  # avg width
  line_height_npc <- pt_to_npc * text_size * 1.2
  max_chars_per_line <- floor(width_npc / char_width_npc)
  max_lines <- floor(height_npc / line_height_npc)
  if (max_lines <= 0 || max_chars_per_line <= 0) return("")
  
  if (abbreviate_text) {
    words <- strsplit(text, "\\s+")[[1]]
    minlength <- if (!is.null(abbr_minlength)) abbr_minlength else max(2, floor(max_chars_per_line / 2))
    words <- abbreviate(words, minlength = minlength)
    text <- paste(words, collapse = " ")
  }
  
  if (wrap) {
    lines <- strwrap(text, width = max_chars_per_line)
    if (truncate && length(lines) > max_lines) {
      lines <- lines[1:max_lines]
      lines[length(lines)] <- paste0(lines[length(lines)], "...")
    }
    return(paste(lines, collapse = "<br>"))
  } else {
    words <- strsplit(text, "\\s+")[[1]]
    word_was_cut <- FALSE
    for (i in seq_along(words)) {
      if (nchar(words[i]) > max_chars_per_line) {
        words[i] <- substr(words[i], 1, max_chars_per_line)
        word_was_cut <- TRUE
      }
    }
    text <- paste(words, collapse = " ")
    if (truncate && word_was_cut) {
      text <- paste0(text, "...")
    }
    return(text)
  }
}
