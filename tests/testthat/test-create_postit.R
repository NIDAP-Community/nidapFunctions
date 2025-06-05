test_that("create_postit runs silently with defaults", {
  expect_invisible(
    suppressMessages(create_postit("Hello world", rstudio = TRUE))
  )
})

test_that("create_postit handles custom color overrides", {
  expect_invisible(
    suppressMessages(
      create_postit(
        text_string = "Color~override test",
        custom_fill_color = "#ABCDEF",
        custom_text_color = "#123456",
        custom_border_color = "#654321",
        font_face = "bold",
        rstudio = TRUE
      )
    )
  )
})

test_that("create_postit supports absolute padding mode", {
  expect_invisible(
    suppressMessages(
      create_postit(
        text_string = "Absolute padding test",
        use_relative_padding = FALSE,
        padding_width = 1,
        padding_height = 0.5,
        rstudio = TRUE
      )
    )
  )
})

test_that("create_postit shows debug guides without error", {
  expect_invisible(
    suppressMessages(
      create_postit(
        text_string = "Guides visible here",
        debug_guides = TRUE,
        rstudio = TRUE
      )
    )
  )
})

test_that("create_postit works with clipped and unclipped background", {
  expect_invisible(
    suppressMessages(create_postit("Clipped background", bg_clipped = TRUE, rstudio = TRUE))
  )
  expect_invisible(
    suppressMessages(create_postit("Unclipped background", bg_clipped = FALSE, rstudio = TRUE))
  )
})

test_that("create_postit handles multi-line layout properly", {
  expect_invisible(
    suppressMessages(create_postit("Test // New line block", rstudio = TRUE))
  )
})

test_that("create_postit returns patchwork object silently", {
  obj <- suppressMessages(
    create_postit("This is an~object", output = "object", rstudio = TRUE)
  )
  expect_s3_class(obj, "gg")
})

test_that("create_postit handles edge case text and small devices", {
  expect_invisible(
    suppressMessages(
      create_postit(
        text_string = "Edgecase test for small devices",
        device_width = 2,
        device_height = 1,
        rstudio = TRUE
      )
    )
  )
})

test_that("create_postit respects min/max line spacing settings", {
  expect_invisible(
    suppressMessages(
      create_postit(
        text_string = "Spacing test with//min~and~max",
        min_line_spacing = 0.5,
        max_line_spacing = 2,
        rstudio = TRUE
      )
    )
  )
})

test_that("create_postit works with different font faces", {
  expect_invisible(
    suppressMessages(
      create_postit(
        text_string = "Font~style~test",
        font_family = "sans",
        font_face = "italic",
        rstudio = TRUE
      )
    )
  )
})

test_that("create_postit generates expected grid plot snapshot", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "postit_grid_output_snapshot",
    fig = function() {
      suppressMessages(
        create_postit(
          "📝 Post-it Snapshot",
          device_width = 5,
          device_height = 2,
          rstudio = TRUE
        )
      )
    }
  )
})
