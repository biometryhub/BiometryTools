#' FAST: overall performance and stability for interpreting Factor Analytic models
#'
#' @param model
#' @param dat
#' @param term
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' JULES COMPLETE
#' }
fast <- function(model, dat = NULL, term = "fa(Site, 4):Genotype", ...) {
  #    dat <- eval(model$call$data)
  str <- strsplit(term, ":")[[1]]
  sterm <- sapply(strsplit(gsub("fa\\(|\\))", "", str[grep("fa", str)]), ","), "[", 1)
  gterm <- str[-grep("fa", str)]
  sfa <- fa.asreml(model, ...)
  scores <- sfa$blups[[term]]$scores
  lvar <- cbind.data.frame(sfa$gammas[[term]]$"rotated loads", sfa$gammas[[term]]$"specific var")
  scores <- do.call("cbind.data.frame", tapply(scores$blupr, scores[[sterm]], function(el) el))
  names(scores) <- ns <- paste("score", 1:ncol(scores), sep = "")
  nk <- dim(scores)[2]
  scores <- cbind.data.frame(levels(dat[[gterm]]), scores)
  names(scores)[1] <- gterm
  sa <- scores[rep(1:nrow(scores), nrow(lvar)), ]
  lvar <- lvar[rep(1:nrow(lvar), each = nrow(scores)), ]
  nl <- paste("loads", 1:(ncol(lvar) - 1), sep = "")
  names(lvar) <- c(nl, "spec.var")
  ls <- cbind.data.frame(rep(levels(dat[[sterm]]), each = nrow(scores)), lvar, sa)
  names(ls)[1] <- sterm
  for (i in 1:nk) {
    ts <- paste("fitted", i, sep = "")
    ls[[ts]] <- ls[[ns[i]]] * ls[[nl[[i]]]]
  }
  print(names(ls))
  ls$CVE <- rowSums(ls[, grep("fitted", names(ls))])
  ls$VE <- ls$CVE + ls[, "spec.var"]
  ls$OP <- mean(ls$loads1) * ls$score1
  ls$dev <- ls$CVE - ls$fitted1
  ls$stab <- tapply(ls$dev^2, ls$Genotype, mean)[as.character(ls$Genotype)]
  ls
}
