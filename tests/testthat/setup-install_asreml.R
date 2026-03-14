expect_file_2 <- function(fn, args, pat, dir = ".", missing = F) {
  x <- do.call(fn, args)
  if (!missing) {
    expect_true(length(list.files(path = dir, pattern = pat)) > 0)
  }
  else {
    expect_true(length(list.files(path = dir, pattern = pat)) == 0)
  }
}
