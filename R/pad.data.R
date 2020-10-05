#' Title
#'
#' @param data
#' @param pattern
#' @param split
#' @param keep
#' @param fill
#'
#' @return
#' @export
#'
#' @examples
pad.data <- function(data, pattern = "Row:Column", split = "Block", keep = 4, fill = NULL) {
  pat <- unlist(strsplit(pattern, ":"))
  if (!(split %in% names(data))) {
    stop("split argument not in data")
  }
  if (!all(pat %in% names(data))) {
    stop("One or more of the variables in pattern argument not in data")
  }
  spd <- split(data, data[[split]])
  spd <- lapply(spd, function(el, pat) {
    temp <- el
    temp <- cbind.data.frame(lapply(temp, function(el) {
      if (is.factor(el)) factor(el) else el
    }))
    temp$add <- "old"
    tabs <- table(temp[[pat[1]]], temp[[pat[2]]])
    wh <- which(tabs == 0, arr.ind = TRUE)
    if (dim(wh)[1] > 0) {
      tp <- temp[1:nrow(wh), ]
      tp <- cbind.data.frame(lapply(tp, function(el) rep(NA, length(el))))
      tp[, keep] <- temp[1:nrow(wh), keep]
      tp[[pat[1]]] <- factor(rownames(tabs)[wh[, 1]])
      tp[[pat[2]]] <- factor(colnames(tabs)[wh[, 2]])
      if (!is.null(fill)) {
        tp[, fill] <- NA
      }
      tp$add <- "new"
      temp <- rbind.data.frame(temp, tp)
    }
    temp
  }, pat)
  ad <- do.call("rbind.data.frame", spd)
  ad[[pat[1]]] <- factor(ad[[pat[1]]], levels = as.character(sort(as.numeric(levels(ad[[pat[1]]])))))
  ad[[pat[2]]] <- factor(ad[[pat[2]]], levels = as.character(sort(as.numeric(levels(ad[[pat[2]]])))))
  ad[order(ad[[pat[1]]], ad[[pat[2]]]), ]
}
