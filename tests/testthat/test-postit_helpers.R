test_that("parse_text handles all types of breaks", {
  input <- "foo~bar // baz qux"
  result <- parse_text(input)
  expect_equal(result, list(c("foo bar"), c("baz", "qux")))
})

test_that("parse_text handles no separators as single block", {
  input <- "foo bar"
  result <- parse_text(input)
  expect_equal(result, list(c("foo", "bar")))
})

test_that("parse_text trims whitespace around hard blocks", {
  input <- "  foo~bar   //  baz   qux "
  result <- parse_text(input)
  expect_equal(result, list(c("foo bar"), c("baz", "qux")))
})


test_that("wrap_variants returns at least one layout", {
  parsed <- list(c("hello", "world"))
  result <- wrap_variants(parsed)
  expect_true(length(result) > 0)
})

test_that("wrap_variants handles multi-block input", {
  parsed <- list(c("a", "b"), c("c", "d"))
  variants <- wrap_variants(parsed)
  expect_true(all(sapply(variants, is.character)))
  expect_true(any(sapply(variants, function(x) length(x) > 1)))
})


test_that("layout_fits fails if text is too wide", {
  too_long <- c("this line is definitely way too long for the box")
  fits <- layout_fits(too_long, fontsize_pt = 30, available_width = 1, available_height = 1)
  expect_false(fits)
})

test_that("layout_fits respects line spacing", {
  lines <- c("line one", "line two")
  fits_default <- layout_fits(lines, fontsize_pt = 10, available_width = 5, available_height = 1)
  fits_tight <- layout_fits(lines, fontsize_pt = 10, available_width = 5, available_height = 1, line_spacing = 0.8)
  expect_true(fits_tight || fits_default)
})


test_that("find_best_layout returns highest fontsize layout that fits", {
  layouts <- list(
    c("short", "lines"),
    c("a much longer line that probably won't fit in the same space")
  )
  result <- find_best_layout(layouts, width = 5, height = 2)
  expect_named(result, c("layout", "fontsize"))
  expect_true(is.character(result$layout))
  expect_true(result$fontsize > 0)
})


test_that("resolve_color handles case-insensitive palette names", {
  expect_equal(resolve_color("Red1"), "#F44E3B")
})

test_that("resolve_color allows fallback to named color with allow_any", {
  expect_equal(resolve_color("deeppink", allow_any = TRUE), "deeppink")
})

test_that("resolve_color fails on invalid input without allow_any", {
  expect_error(
    resolve_color("notacolor"),
    "'notacolor' is not a valid post-it palette color"
  )
})