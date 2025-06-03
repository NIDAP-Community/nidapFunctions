test_that("parse_text splits text on hard blocks and glue words", {
  result <- parse_text("foo~bar // baz qux")
  expect_equal(result, list(c("foo bar"), c("baz", "qux")))
})

test_that("wrap_variants handles mixed fixed and free lines", {
  parsed <- list(c("hello"), c("world", "from", "r"))
  variants <- wrap_variants(parsed)
  expect_true(length(variants) > 1)
  expect_true(any(sapply(variants, function(x) length(x) > 1)))
})

test_that("layout_fits returns TRUE when text fits", {
  result <- layout_fits(
    lines = c("short line"),
    fontsize_pt = 8,
    available_width = 5,
    available_height = 5
  )
  expect_true(result)
})

test_that("find_best_layout returns a valid layout and font size", {
  layouts <- list(c("short line"), c("a bit longer line"))
  result <- find_best_layout(layouts, width = 5, height = 5)
  expect_type(result, "list")
  expect_named(result, c("layout", "fontsize"))
  expect_gt(result$fontsize, 0)
})

test_that("resolve_color returns correct hex for valid palette name", {
  expect_equal(resolve_color("red1"), "#F44E3B")
  expect_equal(resolve_color("Red1"), "#F44E3B")  # case-insensitive
})

# ✅ NEW TEST: allow_any = TRUE should accept non-palette color
test_that("resolve_color allows any color if allow_any = TRUE", {
  expect_equal(resolve_color("deeppink", allow_any = TRUE), "deeppink")
})

# ✅ UPDATED TEST: enforce error on invalid palette name
test_that("resolve_color errors on invalid name without allow_any", {
  expect_error(
    resolve_color("notacolor"),
    "'notacolor' is not a valid post-it palette color"
  )
})
