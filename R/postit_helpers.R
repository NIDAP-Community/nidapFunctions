#' Internal and Exported Utilities for Post-it Rendering
#'
#' These functions support layout, coloring, sizing, and rendering of Post-it
#' notes using grid and ggplot2 graphics.
#'
#' @importFrom grDevices dev.cur
#' @importFrom grid textGrob gpar convertWidth convertHeight grobWidth
#' @importFrom grid grobHeight grid.newpage pushViewport popViewport viewport
#' @importFrom grid unit grid.rect grid.draw editGrob grobTree linesGrob gList
#' @importFrom ggplot2 ggplot geom_text aes theme theme_void ggtitle
#' @importFrom ggplot2 scale_fill_identity geom_tile coord_flip
#' @importFrom scales alpha
#' @importFrom utils  head combn
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("hex", "label_color", "name"))
# ------------------------------------------------------------------
# Color Utilities ----
# ------------------------------------------------------------------

#' Color palette used for Post-it rendering
#'
#' A named character vector of colors used in post-it visuals.
#'
#' @format Named character vector of hex colors.
#' @keywords internal
"postit_palette"

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

#' Resolve a palette or custom color
#'
#' Resolves a color name from the `postit_palette`, or returns the input if
#' `allow_any = TRUE`.
#'
#' @param color Character. Color name or hex code.
#' @param allow_any Logical. If `TRUE`, returns the input color even if not in
#'   the palette.
#'
#' @keywords internal
#' @noRd
resolve_color <- function(color, allow_any = FALSE) {
  color <- tolower(color)
  if (color %in% names(postit_palette))
    return(postit_palette[[color]])
  if (allow_any)
    return(color)
  stop(
    sprintf(
      "'%s' is not a valid post-it palette color.\nValid options are: %s",
      color,
      paste(names(postit_palette), collapse = ", ")
    )
  )
}

#' Display all Post-it palette colors as a grid
#'
#' Generates a visual overview of all defined colors with labels.
#'
#' @return A `ggplot2` plot showing the palette.
#' @export
show_postit_palette <- function() {
  pal <- postit_palette
  n_rows <- 3
  n_cols <- ceiling(length(pal) / n_rows)
  index_matrix <- matrix(seq_along(pal), nrow = n_rows, byrow = FALSE)
  col_order <- as.vector(index_matrix)
  
  df <- data.frame(
    name = names(pal)[col_order],
    hex = unname(pal)[col_order],
    col = rep(1:n_cols, each = n_rows),
    row = rep(n_rows:1, times = n_cols),
    stringsAsFactors = FALSE
  )
  
  hex_to_luminance <- function(hex) {
    rgb <- grDevices::col2rgb(hex) / 255
    c <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    return(0.2126 * c[1, ] + 0.7152 * c[2, ] + 0.0722 * c[3, ])
  }
  
  df$luminance <- sapply(df$hex, hex_to_luminance)
  df$label_color <- ifelse(df$luminance < 0.5, "white", "black")
  
  ggplot(df, aes(x = col, y = row)) +
    geom_tile(
      aes(fill = hex),
      color = "white",
      width = 0.9,
      height = 0.9
    ) +
    geom_text(aes(label = paste0(name, "\n", hex), color = label_color), size = 3) +
    scale_fill_identity() +
    scale_color_identity() +
    theme_void() +
    theme(plot.margin = unit(rep(0.5, 4), "cm")) +
    ggtitle("Post-it Color Palette")
}

# ------------------------------------------------------------------
# Text Layout and Font Sizing ----
# ------------------------------------------------------------------

#' Parse a text string into layout-ready blocks
#'
#' Splits input using hard breaks (`//`), glues words on `~`, and soft breaks on
#' spaces.
#' 
#' @param txt Character. Text to parse.
#' @return List of parsed blocks.
#' @keywords internal
#' @noRd
parse_text <- function(txt) {
  hard_blocks <- strsplit(txt, "//")[[1]]
  parsed <- lapply(hard_blocks, function(block) {
    glued_chunks <- strsplit(trimws(block), "\\s+")[[1]]
    gsub("~", " ", glued_chunks)
  })
  return(parsed)
}

#' Generate layout variants from parsed blocks
#'
#' Computes all line-breaking variants across parsed blocks.
#'
#' @param parsed List. Parsed text from `parse_text()`.
#' @return List of character vectors representing line layouts.
#' @keywords internal
#' @noRd
wrap_variants <- function(parsed) {
  contiguous_splits <- function(words) {
    n <- length(words)
    if (n == 1)
      return(list(list(words)))
    result <- list()
    for (k in 1:n) {
      split_points <- combn(1:(n - 1), k - 1, simplify = FALSE)
      for (splits in split_points) {
        idx <- c(0, splits, n)
        chunks <- mapply(function(i, j)
          words[(i + 1):j], idx[-length(idx)], idx[-1], SIMPLIFY = FALSE)
        result[[length(result) + 1]] <- chunks
      }
    }
    return(result)
  }
  
  per_block_variants <- lapply(parsed, function(block) {
    if (length(block) == 1)
      list(block)
    else
      contiguous_splits(block)
  })
  
  cartesian_product <- function(lists) {
    if (length(lists) == 1)
      return(lists[[1]])
    grid <- expand.grid(lists, stringsAsFactors = FALSE)
    apply(grid, 1, function(row)
      unlist(row, recursive = FALSE), simplify = FALSE)
  }
  
  layout_lists <- cartesian_product(per_block_variants)
  layouts <- lapply(layout_lists, function(lines)
    sapply(lines, paste, collapse = " "))
  return(layouts)
}

#' Check if a text layout fits in given dimensions
#'
#' Measures total height and maximum width of grobs and compares to constraints.
#'
#' @param lines Character vector. Lines of text.
#' @param fontsize_pt Numeric. Font size in points.
#' @param available_width, available_height Numeric. Dimensions in inches.
#' @param line_spacing Numeric. Multiplier for line gap.
#' @return Logical indicating if layout fits.
#' @keywords internal
#' @noRd
layout_fits <- function(lines,
                        fontsize_pt,
                        available_width,
                        available_height,
                        line_spacing = 1.5) {
  grobs <- lapply(lines, function(line) {
    textGrob(line, gp = gpar(fontsize = fontsize_pt))
  })
  widths <- sapply(grobs, function(g)
    convertWidth(grobWidth(g), "inches", valueOnly = TRUE))
  heights <- sapply(grobs, function(g)
    convertHeight(grobHeight(g), "inches", valueOnly = TRUE))
  
  total_height <- if (length(lines) == 1)
    heights
  else
    sum(heights) + (length(lines) - 1) * mean(heights) * (line_spacing - 1)
  return(max(widths) <= available_width &&
           total_height <= available_height)
}

#' Find the largest font and best layout that fits
#'
#' Iterates over layouts and finds the maximum font size that fits given
#' constraints.
#' 
#' @param layouts List of character vectors. Output from `wrap_variants()`.
#' @param width, height Numeric. Available space in inches.
#' @param line_spacing Numeric. Spacing multiplier.
#' @return A list with `layout` and `fontsize`.
#' @keywords internal
#' @noRd
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

#' Create a background grob for Post-it
#'
#' Draws a filled rectangle with optional border and clipping.
#'
#' @param device_width, device_height Numeric. Inches.
#' @param fill_color, fill_alpha Character/Number. Fill color and alpha.
#' @param border_color, border_alpha Character/Number. Border color and alpha.
#' @param border_size Numeric. Stroke width.
#' @param bg_clipped Logical. Clip to viewport.
#'
#' @return A `grobTree` object.
#' @keywords internal
#' @noRd
draw_postit_background <- function(device_width,
                                   device_height,
                                   fill_color,
                                   fill_alpha,
                                   border_color,
                                   border_alpha,
                                   border_size = 1,
                                   bg_clipped = FALSE) {
  vp <- if (bg_clipped) {
    viewport(
      width = unit(device_width, "inches"),
      height = unit(device_height, "inches")
    )
  } else
    NULL
  
  bg_grob <- rectGrob(
    x = 0.5,
    y = 0.5,
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    gp = gpar(
      fill = scales::alpha(fill_color, fill_alpha),
      col = scales::alpha(border_color, border_alpha),
      lwd = border_size
    )
  )
  grobTree(bg_grob, vp = vp)
}

#' Draw vertically-aligned text grobs
#'
#' Positions text lines within a vertical area using spacing rules.
#'
#' @param lines Character vector of text lines.
#' @param fontsize Numeric. Font size.
#' @param text_color Character. Text color.
#' @param font_family, font_face Character. Font settings.
#' @param available_height_npc Numeric. Height as fraction of total viewport.
#' @param offset_top_npc Numeric. Top margin offset in NPC.
#' @param min_line_spacing, max_line_spacing Numeric or NULL. Spacing limits.
#'
#' @return A list of vertically-positioned grobs.
#' @keywords internal
#' @noRd
draw_text_lines <- function(lines,
                            fontsize,
                            text_color,
                            font_family = "",
                            font_face = "plain",
                            available_height_npc,
                            offset_top_npc,
                            min_line_spacing = NULL,
                            max_line_spacing = NULL) {
  is_single_line <- length(lines) == 1
  
  grobs <- lapply(lines, function(line) {
    textGrob(
      label = line,
      gp = gpar(
        fontsize = fontsize,
        col = text_color,
        fontfamily = font_family,
        fontface = font_face
      )
    )
  })
  
  heights <- sapply(grobs, function(g)
    convertHeight(grobHeight(g), "npc", valueOnly = TRUE))
  
  if (is_single_line) {
    y_positions <- 1 - offset_top_npc - available_height_npc / 2
  } else {
    first_top <- 1 - offset_top_npc
    last_bottom <- 1 - offset_top_npc - available_height_npc
    total_text_height <- sum(heights)
    vertical_space <- first_top - last_bottom
    gaps <- length(lines) - 1
    raw_gap <- (vertical_space - total_text_height) / max(gaps, 1)
    if (!is.null(min_line_spacing))
      raw_gap <- max(raw_gap, min_line_spacing)
    if (!is.null(max_line_spacing))
      raw_gap <- min(raw_gap, max_line_spacing)
    line_offsets <- cumsum(c(0, head(heights + raw_gap, -1)))
    top_y <- first_top - heights[1] / 2
    y_positions <- top_y - line_offsets
  }
  
  positioned <- mapply(
    function(g, y)
      editGrob(g, y = unit(y, "npc"), just = "center"),
    grobs,
    y_positions,
    SIMPLIFY = FALSE
  )
  
  return(positioned)
}
