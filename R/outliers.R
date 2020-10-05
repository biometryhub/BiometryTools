#' Downweight outliers
#'
#' @param data
#' @param model
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
outlier.down <- function(data, model, cutoff = 3) {
  ss <- names(model)
  inds <- 1:nrow(data)
  for (i in 1:length(ss)) {
    str <- abs(model[[ss[i]]]$aom$R[, 2])
    r <- str > cutoff
    wh <- inds[r]
    wh <- wh[!is.na(wh)]
    if (length(wh)) {
      ps <- paste(ss[i], "o", sep = ".")
      num <- 0
      if (length(wt <- grep(ps, names(data)))) {
        num <- sapply(strsplit(names(data)[wt], "\\."), function(el) el[length(el)])
        num <- as.numeric(num[length(num)])
      }
      print(wh)
      for (j in 1:length(wh)) {
        nam <- paste(ps, j + num, sep = ".")
        v <- rep(0, nrow(data))
        v[wh[j]] <- 1
        data[[nam]] <- v
      }
    }
  }
  data
}

#' Outlier removal function
#'
#' @param data
#' @param model
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
outlier.rem <- function(data, model, cutoff = 3) {
  ss <- names(model)
  inds <- 1:nrow(data)
  out <- rep(FALSE, length(model))
  names(out) <- ss
  for (i in 1:length(ss)) {
    trait <- data[[ss[i]]]
    str <- abs(model[[ss[i]]]$aom$R[, 2])
    r <- str > cutoff
    wh <- inds[r]
    wh <- wh[!is.na(wh)]
    if (length(wh)) {
      print(wh)
      data[[ss[i]]][wh] <- NA
      out[i] <- TRUE
    }
  }
  list(data = data, out = out)
}
