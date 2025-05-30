#' Internal helper to process and format text for create_postit()
#'
#' This function formats the input text string by applying optional abbreviation,
#' wrapping, and truncation. If `wrap = FALSE` and any word exceeds the character
#' limit (`text_width`), it is shortened and a trailing `"..."` is added if
#' truncation is enabled and line limit conditions are met.
#'
#' @param text The input character string.
#' @param wrap Logical. Whether to wrap the text to multiple lines.
#' @param text_width Integer. Maximum characters per line or per word.
#' @param max_lines Integer. Maximum number of lines. If 0, returns an empty string.
#' @param truncate Logical. Whether to truncate overflow and add an ellipsis.
#' @param abbreviate_text Logical. If TRUE, each word is abbreviated before further processing.
#' @param abbr_minlength Integer. Minimum abbreviation length. If NULL, estimated from text_width.
#'
#' @return A processed character string suitable for rendering in a ggplot label.
#' @keywords internal
process_postit_text <- function(
    text,
    wrap = TRUE,
    text_width = 40,
    max_lines = 5,
    truncate = TRUE,
    abbreviate_text = FALSE,
    abbr_minlength = NULL
) {
  if (abbreviate_text) {
    words <- strsplit(text, "\\s+")[[1]]
    minlength <- if (!is.null(abbr_minlength)) abbr_minlength else max(2, floor(text_width / 2))
    words <- abbreviate(words, minlength = minlength)
    text <- paste(words, collapse = " ")
  }
  
  if (wrap) {
    lines <- strwrap(text, width = text_width)
    if (max_lines == 0) return("")
    if (truncate && length(lines) > max_lines) {
      lines <- c(lines[1:max_lines], "...")
    }
    text <- paste(lines, collapse = "<br>")
  } else {
    words <- strsplit(text, "\\s+")[[1]]
    word_was_cut <- FALSE
    for (i in seq_along(words)) {
      if (nchar(words[i]) > text_width) {
        words[i] <- substr(words[i], 1, text_width)
        word_was_cut <- TRUE
      }
    }
    text <- paste(words, collapse = " ")
    if (truncate && max_lines == 1 && word_was_cut) {
      text <- paste0(text, "...")
    }
    if (max_lines == 0) {
      text <- ""
    }
  }
  
  text
}
