#' Title
#'
#' @param data
#' @param pattern
#' @param match
#' @param split
#' @param pad
#' @param keep
#' @param fill
#'
#' @return
#' @export
#'
#' @examples
extract <- function(data, pattern = "Row:Column", match = "DH", split = "Block", pad = TRUE, keep = 4, fill = NULL) {
  pat <- unlist(strsplit(pattern, ":"))
  if (!(split %in% names(data))) {
    stop("split argument not in data")
  }
  if (!all(pat %in% names(data))) {
    stop("One or more of the variables in pattern argument not in data")
  }
  spd <- split(data, data[[split]])
  spd <- lapply(spd, function(el, match, pat, pad) {
    temp <- el[as.character(el$Type) %in% match, ]
    print(dim(temp))
    rr <- range(as.numeric(as.character(temp[, pat[1]])))
    rc <- range(as.numeric(as.character(temp[, pat[2]])))
    print(rr)
    print(rc)
    elr <- (1:nrow(el))[el[[pat[1]]] %in% as.character(rr[1]:rr[2])]
    elc <- (1:nrow(el))[el[[pat[2]]] %in% as.character(rc[1]:rc[2])]
    ela <- intersect(elr, elc)
    temp <- el[ela[order(ela)], ]
    temp <- cbind.data.frame(lapply(temp, function(el) {
      if (is.factor(el)) factor(el) else el
    }))
    if (pad) {
      temp$add <- "old"
      tabs <- table(temp[[pat[1]]], temp[[pat[2]]])
      wh <- which(tabs == 0, arr.ind = TRUE)
      if (length(wh)) {
        whn <- pmatch(pat, names(temp))
        tp <- temp[1:nrow(wh), ]
        tp <- cbind.data.frame(lapply(tp, function(el) rep(NA, length(el))))
        tp[, keep] <- temp[1:nrow(wh), keep]
        tp[[pat[1]]] <- factor(rownames(tabs)[wh[, 1]])
        tp[[pat[2]]] <- factor(colnames(tabs)[wh[, 2]])
        if (!is.null(fill)) {
          tp[, fill] <- "Blank"
        }
        tp$add <- "new"
        temp <- rbind.data.frame(temp, tp)
      }
    }
    temp
  }, match, pat, pad)
  ad <- do.call("rbind.data.frame", spd)
  ad[[pat[1]]] <- factor(ad[[pat[1]]], levels = as.character(sort(as.numeric(levels(ad[[pat[1]]])))))
  ad[[pat[2]]] <- factor(ad[[pat[2]]], levels = as.character(sort(as.numeric(levels(ad[[pat[2]]])))))
  ad[order(ad[[pat[1]]], ad[[pat[2]]]), ]
}
