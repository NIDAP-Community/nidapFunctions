#' Internal and Exported Utilities for Post-it Rendering
#'
#' These functions support layout, coloring, sizing, and rendering of post-it
#' notes using grid and ggplot2 graphics.
#'
#' @importFrom grid textGrob gpar convertWidth convertHeight grobWidth
#' @importFrom grid grobHeight grid.newpage pushViewport popViewport viewport
#' @importFrom grid unit grid.rect grid.draw editGrob
#' @importFrom ggplot2 ggplot geom_text aes theme theme_void ggtitle
#' @importFrom ggplot2 scale_fill_identity geom_tile coord_flip
#' @importFrom scales alpha
#' @importFrom utils  head
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("hex", "label_color", "name"))

# ------------------------------------------------------------------
# Color Utilities ----
# ------------------------------------------------------------------

#' Named Color Palette
#'
#' A named character vector containing predefined hex color codes.
#'
#' @format Named character vector.
#' @keywords internal
postit_palette <- c(
  darkgray1 = "#4D4D4D",
  darkgray2 = "#333333",
  black = "#000000",
  gray1 = "#999999",
  gray2 = "#808080",
  gray3 = "#666666",
  white = "#FFFFFF",
  lightgray1 = "#CCCCCC",
  lightgray2 = "#B3B3B3",
  red1 = "#F44E3B",
  red2 = "#D33115",
  red3 = "#9F0500",
  orange1 = "#FE9200",
  orange2 = "#E27300",
  orange3 = "#C45100",
  yellow1 = "#FCDC00",
  yellow2 = "#FCC400",
  yellow3 = "#FB9E00",
  yellowgreen1 = "#DBDF00",
  yellowgreen2 = "#B0BC00",
  yellowgreen3 = "#808900",
  green1 = "#A4DD00",
  green2 = "#68BC00",
  green3 = "#194D33",
  teal1 = "#68CCCA",
  teal2 = "#16A5A5",
  teal3 = "#0C797D",
  blue1 = "#73D8FF",
  blue2 = "#009CE0",
  blue3 = "#0062B1",
  purple1 = "#AEA1FF",
  purple2 = "#7B64FF",
  purple3 = "#653294",
  magenta1 = "#FDA1FF",
  magenta2 = "#FA28FF",
  magenta3 = "#AB149E"
)

#' @noRd
#' @keywords internal
resolve_color <- function(color, allow_any = FALSE) {
  color <- tolower(color)
  
  if (color %in% names(postit_palette)) {
    return(postit_palette[[color]])
  }
  
  if (allow_any) {
    return(color)
  }
  
  stop(
    sprintf(
      "'%s' is not a valid post-it palette color.\nValid options are: %s",
      color,
      paste(names(postit_palette), collapse = ", ")
    )
  )
}
#' Display Named Post-it Color Palette
#'
#' Visualizes all available named colors in the `postit_palette`, with labels and
#' hex codes.
#'
#' @return A ggplot2 object displaying the palette grid.
#' @export
#'
#' @examples
#' show_postit_palette()
show_postit_palette <- function() {
  pal <- postit_palette
  n_rows <- 3
  n_cols <- ceiling(length(pal) / n_rows)
  
  # Create column-first order
  index_matrix <- matrix(seq_along(pal), nrow = n_rows, byrow = FALSE)
  col_order <- as.vector(index_matrix)
  
  # Create data frame
  df <- data.frame(
    name = names(pal)[col_order],
    hex = unname(pal)[col_order],
    col = rep(1:n_cols, each = n_rows),
    row = rep(n_rows:1, times = n_cols),
    stringsAsFactors = FALSE
  )
  
  # Calculate luminance and label color
  hex_to_luminance <- function(hex) {
    rgb <- grDevices::col2rgb(hex) / 255
    c <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    return(0.2126 * c[1, ] + 0.7152 * c[2, ] + 0.0722 * c[3, ])
  }
  
  df$luminance <- sapply(df$hex, hex_to_luminance)
  df$label_color <- ifelse(df$luminance < 0.5, "white", "black")
  
  ggplot2::ggplot(df, ggplot2::aes(x = col, y = row)) +
    ggplot2::geom_tile(
      aes(fill = hex),
      color = "white",
      width = 0.9,
      height = 0.9
    ) +
    ggplot2::geom_text(
      aes(label = paste0(name, "\n", hex), color = label_color), size = 3) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = grid::unit(rep(0.5, 4), "cm")) +
    ggplot2::ggtitle("Post-it Color Palette")
}


# ------------------------------------------------------------------
# Text Parsing & Wrapping ----
# ------------------------------------------------------------------

#' @noRd
#' @keywords internal
parse_text <- function(txt) {
  hard_blocks <- strsplit(txt, "//")[[1]]
  parsed <- lapply(hard_blocks, function(block) {
    glued_chunks <- strsplit(trimws(block), "\\s+")[[1]]
    gsub("~", " ", glued_chunks)
  })
  return(parsed)
}

#' @noRd
#' @keywords internal
wrap_variants <- function(parsed) {
  fixed_lines <- list()
  free_words <- c()
  
  for (group in parsed) {
    if (length(group) == 1) {
      fixed_lines <- append(fixed_lines, list(group))
    } else {
      free_words <- c(free_words, group)
    }
  }
  
  layouts <- list()
  max_lines <- length(free_words)
  
  if (max_lines == 0) {
    layouts[[1]] <- sapply(fixed_lines, paste, collapse = " ")
    return(layouts)
  }
  
  for (k in 1:max_lines) {
    chunks <- rep(1:k, length.out = max_lines)
    wrapped <- tapply(free_words, chunks, paste, collapse = " ")
    layout <- c(sapply(fixed_lines, paste, collapse = " "), unname(wrapped))
    layouts[[length(layouts) + 1]] <- layout
  }
  
  return(layouts)
}


# ------------------------------------------------------------------
# Layout Fitting & Font Sizing ----
# ------------------------------------------------------------------

#' @noRd
#' @keywords internal
layout_fits <- function(lines,
                        fontsize_pt,
                        available_width,
                        available_height,
                        line_spacing = 1.5) {
  grobs <- lapply(lines, function(line) {
    grid::textGrob(line, gp = grid::gpar(fontsize = fontsize_pt))
  })
  
  widths <- sapply(grobs, function(g) {
    grid::convertWidth(grid::grobWidth(g), "inches", valueOnly = TRUE)
  })
  
  heights <- sapply(grobs, function(g) {
    grid::convertHeight(grid::grobHeight(g), "inches", valueOnly = TRUE)
  })
  
  if (length(lines) == 1) {
    total_height <- heights
  } else {
    total_height <-
      sum(heights) + (length(lines) - 1) * mean(heights) * (line_spacing - 1)
  }
  
  return(max(widths) <= available_width &&
           total_height <= available_height)
}

#' @noRd
#' @keywords internal
find_best_layout <- function(layouts, width, height, line_spacing = 1.5) {
  best_fontsize <- 0
  best_layout <- NULL
  
  for (lines in layouts) {
    low <- 1
    high <- 500
    local_best <- NA
    
    while (low <= high) {
      mid <- floor((low + high) / 2)
      if (layout_fits(lines, mid, width, height, line_spacing)) {
        local_best <- mid
        low <- mid + 1
      } else {
        high <- mid - 1
      }
    }
    
    if (!is.na(local_best) && local_best > best_fontsize) {
      best_fontsize <- local_best
      best_layout <- lines
    }
  }
  
  return(list(layout = best_layout, fontsize = best_fontsize))
}


# ------------------------------------------------------------------
# Drawing Utilities ----
# ------------------------------------------------------------------

#' @noRd
#' @keywords internal
draw_background <- function(device_width,
                            device_height,
                            fill_color,
                            fill_alpha,
                            border_color,
                            border_alpha) {
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    width = grid::unit(device_width, "inches"),
    height = grid::unit(device_height, "inches")
  ))
  
  grid::grid.rect(
    x = 0.5,
    y = 0.5,
    width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"),
    gp = grid::gpar(
      fill = scales::alpha(fill_color, fill_alpha),
      col  = scales::alpha(border_color, border_alpha)
    )
  )
}

#' @noRd
#' @keywords internal
draw_text_lines <- function(lines,
                            font_size,
                            color,
                            width,
                            height,
                            line_spacing = 1.5,
                            font_family = "",
                            font_face = "plain") {
  grid::pushViewport(grid::viewport(
    x = 0.5,
    y = 0.5,
    width = grid::unit(width, "inches"),
    height = grid::unit(height, "inches")
  ))
  
  grobs <- lapply(lines, function(line) {
    grid::textGrob(
      line,
      gp = grid::gpar(
        fontsize = font_size,
        col = color,
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
  
  if (length(heights) > 1) {
    top_offset <- (line_spacing - 1) * mean(heights) / 2
  } else {
    top_offset <- 0
  }
  
  line_offsets <- cumsum(c(0, head(spaced_heights, -1)))
  y_positions <- 0.5 + (total_height / 2) - line_offsets - top_offset
  
  for (i in seq_along(grobs)) {
    grid::grid.draw(grid::editGrob(
      grobs[[i]],
      y = grid::unit(y_positions[i], "npc"),
      just = "top"
    ))
  }
  
  grid::popViewport()
}
