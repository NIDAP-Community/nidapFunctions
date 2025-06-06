#' Generate a Post-IT style graphic with optimized text sizing and layout
#'
#' Automatically computes the best text layout and maximum font size to render
#' the input string within the specified dimensions. Text is parsed using soft
#' breaks (spaces), hard breaks (`/`), and linked words (`~`) to guide line
#' wrapping. The result is a centered, readable graphic with aligned and
#' balanced text lines that adapt to content and constraints.
#'
#' @param text_string Character. Input string for the note.
#' @param device_width Numeric. Width of the device in inches.
#' @param device_height Numeric. Height of the device in inches.
#' @param fill_color Character. Fill color from palette or hex code.
#' @param text_color Character. Text color from palette or hex code.
#' @param border_color Character. Border color from palette or hex code.
#' @param fill_alpha Numeric. Alpha transparency for the fill color.
#' @param border_alpha Numeric. Alpha transparency for the border color.
#' @param border_size Numeric. Border line width in points.
#' @param custom_fill_color Character. Custom fill color (overrides palette).
#' @param custom_border_color Character. Custom border color (overrides
#'   palette).
#' @param custom_text_color Character. Custom text color (overrides palette).
#' @param padding_width Numeric. Horizontal padding size (absolute or relative).
#' @param padding_height Numeric. Vertical padding size (absolute or relative).
#' @param use_relative_padding Logical. Whether padding is relative to device
#'   size.
#' @param font_family Character. Font family for the text.
#' @param font_face Character. Font face (e.g., "plain", "bold", "italic").
#' @param min_line_spacing Numeric or NULL. Minimum spacing between lines.
#' @param max_line_spacing Numeric or NULL. Maximum spacing between lines.
#' @param output Character. Either `"plot"` or `"object"` (returns patchwork).
#' @param rstudio Logical. If `TRUE`, closes RStudio graphics devices before
#'   rendering.
#' @param bg_clipped Logical. If `TRUE`, background is clipped to the viewport.
#' @param debug_guides Logical. If `TRUE`, shows guide lines for alignment and
#'   padding.
#' @param verbose Logical. If `TRUE`, prints selected layout and font size to
#'   console.
#'
#' @return A grid drawing or patchwork object, depending on `output`.
#'
#' @importFrom grid unit viewport grobTree rectGrob linesGrob gpar textGrob
#'   grobWidth grobHeight convertWidth convertHeight editGrob pushViewport
#'   popViewport grid.draw grid.newpage gList
#' @importFrom grDevices dev.off
#' @importFrom patchwork wrap_elements
#' @importFrom scales alpha
#' @export
#' 
#' @seealso [postit_palette], [show_postit_palette()]

#' @examples
#' smart_postit(
#'   text_string = "\u270D/Post-IT",
#'   fill_color = "yellow1",
#'   text_color = "black",
#'   font_face = "plain",
#'   device_width = 6,
#'   device_height = 4,
#'   rstudio = TRUE,
#'   verbose = FALSE
#' )

smart_postit <- function(
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
    font_family = "",
    font_face = "plain",
    min_line_spacing = NULL,
    max_line_spacing = NULL,
    output = c("plot", "object"),
    rstudio = FALSE,
    bg_clipped = TRUE,
    debug_guides = FALSE,
    verbose = TRUE
) {
  output <- match.arg(output)
  
  # --- Resolve Colors ---
  fill_resolved   <- if (!is.null(custom_fill_color)) custom_fill_color else resolve_color(fill_color)
  border_resolved <- if (!is.null(custom_border_color)) custom_border_color else resolve_color(border_color)
  text_resolved   <- if (!is.null(custom_text_color)) custom_text_color else resolve_color(text_color)
  
  # --- Text Layout ---
  parsed   <- parse_text(text_string)
  layouts  <- wrap_variants(parsed)
  
  # --- Padding and Inner Box Dimensions ---
  pad_w <- if (use_relative_padding) device_width * padding_width else padding_width
  pad_h <- if (use_relative_padding) device_height * padding_height else padding_height
  inner_width  <- device_width - 2 * pad_w
  inner_height <- device_height - 2 * pad_h
  
  # --- Best Font/Layout ---
  best <- find_best_layout(layouts, inner_width, inner_height)
  
  if (verbose) {
    message("Best font size: ", best$fontsize, " pt")
    message("Text layout:\n", paste(best$layout, collapse = "\n"))
  }
  
  available_height_npc <- inner_height / device_height
  offset_top_npc <- pad_h / device_height
  
  # --- Viewport Setup ---
  outer_vp <- grid::viewport(
    width = grid::unit(device_width, "inches"),
    height = grid::unit(device_height, "inches")
  )
  
  # --- Background ---
  bg_grob <- draw_postit_background(
    device_width = device_width,
    device_height = device_height,
    fill_color = fill_resolved,
    fill_alpha = fill_alpha,
    border_color = border_resolved,
    border_alpha = border_alpha,
    border_size = border_size,
    bg_clipped = bg_clipped
  )
  
  # --- Text ---
  text_grobs <- draw_text_lines(
    lines = best$layout,
    fontsize = best$fontsize,
    text_color = text_resolved,
    font_family = font_family,
    font_face = font_face,
    available_height_npc = available_height_npc,
    offset_top_npc = offset_top_npc,
    min_line_spacing = min_line_spacing,
    max_line_spacing = max_line_spacing
  )
  
  # --- Optional Debug Guides ---
  guide_grobs <- if (debug_guides) {
    grid::grobTree(
      grid::rectGrob(
        x = 0.5, y = 0.5,
        width = unit(1 - 2 * pad_w / device_width, "npc"),
        height = unit(1 - 2 * pad_h / device_height, "npc"),
        gp = grid::gpar(fill = scales::alpha("grey", 0.08), col = NA)
      ),
      grid::linesGrob(
        x = c(0, 1), y = rep(1 - offset_top_npc - available_height_npc / 2, 2),
        gp = grid::gpar(col = "red", lty = 2)
      ),
      grid::linesGrob(
        x = c(0, 1), y = rep(1 - offset_top_npc, 2),
        gp = grid::gpar(col = "blue", lty = 3)
      ),
      grid::linesGrob(
        x = c(0, 1), y = rep(1 - offset_top_npc - available_height_npc, 2),
        gp = grid::gpar(col = "blue", lty = 3)
      )
    )
  } else {
    NULL
  }
  
  # --- Compose Layers ---
  composed <- if (bg_clipped) {
    grid::grobTree(
      bg_grob,
      if (!is.null(guide_grobs)) guide_grobs,
      grobs = do.call(grid::gList, text_grobs),
      vp = outer_vp
    )
  } else {
    grid::grobTree(
      bg_grob,
      grid::grobTree(
        if (!is.null(guide_grobs)) guide_grobs,
        do.call(grid::gList, text_grobs),
        vp = outer_vp
      )
    )
  }
  
  # --- Render or Return ---
  if (output == "object") {
    if (rstudio && dev.cur() > 1) grDevices::dev.off()
    return(invisible(patchwork::wrap_elements(composed)))
  } else {
    if (rstudio) grid.newpage()
    pushViewport(outer_vp)
    grid.draw(composed)
    popViewport()
    invisible(NULL)
  }
}