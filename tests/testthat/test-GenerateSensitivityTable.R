pmt <- function(prinical, rate) {
  prinical * (rate/12)/(1-(1+rate/12)^-360)
}

test_that("GenerateSensitivityTable returns a list for normal output", {
  expect_type(GenerateSensitivityTable(atan2), "list")
})

test_that("GenerateSensitivityTable produces a warning for NaN", {
  expect_warning(GenerateSensitivityTable(pmt))
})

test_that("GenerateSensitivityTable returns a list for normal output with parameters", {
  expect_type(GenerateSensitivityTable(pmt, 340000, .06), "list")
  expect_type(GenerateSensitivityTable(pmt, 340000, .06, 1, 1, 10, 10), "list")
  expect_type(GenerateSensitivityTable(pmt, 340000, .06, 10000, .0025, 3, 3), "list")
  expect_type(GenerateSensitivityTable(pmt, 340000, .06, -10000, .0025, 3, 3), "list")
  expect_type(GenerateSensitivityTable(pmt, 340000, .06, 10000, -.0025, 3, 3), "list")
  expect_type(GenerateSensitivityTable(pmt, 340000, .06, -10000, -.0025, 3, 3), "list")
  expect_type(GenerateSensitivityTable(pmt, 340000, .06, 10000, .0025, 3, 3, 2), "list")
  expect_type(GenerateSensitivityTable(pmt, 340000, .06, 10000, .0025, 3, 3, 2, 'number'), "list")
  expect_length(GenerateSensitivityTable(pmt, 340000, .06, 10000, .0025, 3, 3, 2, 'number', FALSE), 49)
})

test_that("GenerateSensitivityTable produces errors for invalid inputs", {
  expect_error(GenerateSensitivityTable(min), "The formula must have exactly two variables.")
  expect_error(GenerateSensitivityTable(pmt, 340000, 0.06, 0.1, 0.1, 1, 1),
               "The base parameters are too big and step size is too small. Please adjust your inputs.")
  expect_error(GenerateSensitivityTable(pmt, 340000, 0.06, 10000, .0025, 0, 0),
               "Please ensure all size parameters are greater that zero.")
  expect_error(GenerateSensitivityTable(pmt, 340000, 0.06, 10000, .0025, 0, 1),
               "Please ensure all size parameters are greater that zero.")
  expect_error(GenerateSensitivityTable(pmt, 340000, 0.06, 10000, .0025, 1, -1),
               "Please ensure all size parameters are greater that zero.")
  expect_error(GenerateSensitivityTable(pmt, 340000, 0.06, 10000, .0025, 3.1, 3),
               "Please ensure you are passing integers for steps and nDigits.")
  expect_error(GenerateSensitivityTable(pmt, 340000, 0.06, 10000, .0025, 3, 3.1),
               "Please ensure you are passing integers for steps and nDigits.")
  expect_error(GenerateSensitivityTable(pmt, 340000, 0.06, 10000, .0025, 3.1, 3.1),
               "Please ensure you are passing integers for steps and nDigits.")
  expect_error(GenerateSensitivityTable(pmt, 340000, 0.06, 10000, .0025, 3, 3, 2.1),
               "Please ensure you are passing integers for steps and nDigits.")
  expect_error(GenerateSensitivityTable(pmt, 340000, .06, 10000, .0025, 100, 100, 2, 2),
               "Your input of 2 is not number or percent. Please enter a valid argument.")
  expect_error(GenerateSensitivityTable(pmt, 340000, .06, 10000, .0025, 3, 3, 2, 'foo'),
               "Your input of foo is not number or percent. Please enter a valid argument.")
})
