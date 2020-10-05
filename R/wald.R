#' Title
#'
#' @param object
#' @param cc
#' @param keep.fac
#'
#' @return
#' @export
#'
#' @examples
wald.test.asreml <- function(object, cc, keep.fac = TRUE) {
  if (oldClass(object) != "asreml") {
    stop("Requires an object of class asreml\n")
  }
  if (is.null(object$Cfixed)) {
    warning("Requires C matrix from model object. Refitting test model with argument \"Cfixed = T\"\n")
    asreml.options(Cfixed = TRUE)
    object <- update(object)
  }
  vrb <- as.matrix(object$Cfixed)
  tau <- c(object$coefficients$fixed)
  names(tau) <- rownames(object$coefficients$fixed)
  nc <- length(tau)
  sigma2 <- object$sigma2
  vrb <- vrb / sigma2
  ccnams <- names(tau)
  zdf <- cdf <- NULL
  cc <- lapply(cc, function(el, ccnams) {
    if (!all(names(el) %in% c("coef", "type", "comp", "group"))) {
      stop("Inappropriately named argument for comparison object.")
    }
    if (is.numeric(el$coef)) {
      if (max(el$coef) > length(ccnams)) {
        stop("coefficient subscript out of bounds")
      }
      names(el$coef) <- ccnams[el$coef]
    }
    else {
      if (any(is.na(pmatch(el$coef, ccnams)))) {
        stop("Names in contrast do not match the names of coefficients of object")
      }
      temp <- pmatch(el$coef, ccnams)
      names(temp) <- el$coef
      el$coef <- temp
    }
    el
  }, ccnams)
  ## split contrasts and other available tests
  ctype <- unlist(lapply(cc, function(el) el$type))
  if (!all(ctype %in% c("con", "zero"))) {
    stop("Contrast types must be either \"con\" for treatment comparisons or \"zero\" for testing zero equality")
  }
  cons <- cc[ctype %in% "con"]
  zero <- cc[ctype %in% "zero"]
  cse <- ctau <- zwtest <- cwtest <- zpval <- c()
  if (length(cons)) {
    CRows <- lapply(cons, function(el, nc) {
      if (length(el) < 3) {
        con <- contr.helmert(length(el$coef))[, (length(el$coef) - 1)]
        names(con) <- cnam <- names(el$coef)
        cat("Warning: default contrast being taken for", cnam, "is", con, "\n")
        row <- rep(0, nc)
        row[el$coef] <- con
        row
      }
      else {
        if (is.matrix(el$comp)) {
          if (length(el$coef) != ncol(el$comp)) {
            stop("Length of contrast does not match the number of specified coefficients")
          }
          cons <- split(el$comp, 1:nrow(el$comp))
          rows <- lapply(cons, function(ell, first = el$coef, nc) {
            row <- rep(0, nc)
            row[first] <- ell
            row
          }, first = el$coef, nc)
          rows <- unlist(rows, use.names = F)
          matrix(rows, nrow = nrow(el$comp), byrow = T)
        }
        else {
          if (length(el$coef) != length(el$comp)) {
            stop("Length of contrast does not match the number of specified coefficients")
          }
          row <- rep(0, nc)
          row[el$coef] <- el$comp
          row
        }
      }
    }, nc)
    Cmat <- do.call("rbind", CRows)
    if (!keep.fac) {
      ccnams <- substring(ccnams, regexpr("\\_", ccnams) + 1, nchar(ccnams))
    }
    cnam <- lapply(split(Cmat, 1:nrow(Cmat)), function(el, ccnams) {
      namr <- ccnams[ifelse(el < 0, T, F)]
      naml <- ccnams[ifelse(el > 0, T, F)]
      c(paste(naml, collapse = ":"), paste(namr, collapse = ":"))
    }, ccnams)
    Cnam <- do.call("rbind", cnam)
    gnams <- lapply(cons, function(el) {
      if (!is.null(el$group)) {
        if (!any(names(el$group) %in% c("left", "right"))) {
          stop("group names must be \"left\" and \"right\".")
        }
        if (is.null(el$group$left)) {
          if (is.matrix(el$comp)) {
            el$group$left <- rep(NA, nrow(el$comp))
          } else {
            el$group$left <- NA
          }
        } else {
          if (is.matrix(el$comp)) {
            if (length(el$group$left) == 1) {
              el$group$left <- rep(el$group$left, nrow(el$comp))
            }
            if (length(el$group$left) != nrow(el$comp)) {
              stop("No. of group names do not match the number of comparisons in object")
            }
          }
        }
        if (is.null(el$group$right)) {
          if (is.matrix(el$comp)) {
            el$group$right <- rep(NA, nrow(el$comp))
          } else {
            el$group$right <- NA
          }
        } else {
          if (is.matrix(el$comp)) {
            if (length(el$group$right) == 1) {
              el$group$right <- rep(el$group$right, nrow(el$comp))
            }
            if (length(el$group$right) != nrow(el$comp)) {
              stop("No. of group names do not match the number of comparisons in object")
            }
          }
        }
      } else {
        if (is.matrix(el$comp)) {
          el$group$left <- el$group$right <- rep(NA, nrow(el$comp))
        } else {
          el$group$left <- el$group$right <- NA
        }
      }
      cbind(el$group$left, el$group$right)
    })
    Gnam <- do.call("rbind", gnams)
    Cnam[!is.na(Gnam[, 1]), 1] <- Gnam[!is.na(Gnam[, 1]), 1]
    Cnam[!is.na(Gnam[, 2]), 2] <- Gnam[!is.na(Gnam[, 2]), 2]
    for (i in 1:nrow(Cmat)) {
      varmat <- sum(Cmat[i, ] * crossprod(vrb, t(Cmat)[, i]))
      cse[i] <- sqrt(varmat * sigma2)
      ctau[i] <- sum(Cmat[i, ] * tau)
      cwtest[i] <- (ctau[i] / cse[i])^2
    }
    cdf <- data.frame(
      wald = round(cwtest, 6), pval = round(1 - pchisq(cwtest, 1), 6),
      coef = round(ctau, 6), se = round(cse, 6)
    )
    cat("\nWald Tests: Comparisons\n\n")
    attr(cdf, "names") <- c("Wald Statistic", "P-Value", "Cont. Coef.", "Std. Error")
    attr(cdf, "row.names") <- paste(Cnam[, 1], Cnam[, 2], sep = " vs ")
    oldClass(cdf) <- "data.frame"
  }
  if (length(zero)) {
    ZRows <- lapply(zero, function(el, nc) {
      rows <- rep(rep(0, nc), length(el$coef))
      dum <- seq(0, (length(el$coef) - 1) * nc, by = nc)
      rows[el$coef + dum] <- 1
      matrix(rows, nrow = length(el$coef), byrow = T)
    }, nc)
    znam <- unlist(lapply(zero, function(el, ccnams) {
      if (is.null(el$group)) {
        paste(ccnams[el$coef], collapse = ":")
      } else {
        el$group
      }
    }, ccnams))
    if (any(table(znam) > 1)) {
      stop("Duplicate names in group structures for zero equality tests.")
    }
    for (i in 1:length(ZRows)) {
      varmat <- ZRows[[i]] %*% crossprod(vrb, t(ZRows[[i]]))
      Ctau <- ZRows[[i]] %*% tau
      zwtest[i] <- sum(Ctau * crossprod(solve(varmat), Ctau)) / sigma2
      zpval[i] <- 1 - pchisq(zwtest[i], nrow(ZRows[[i]]))
    }
    zdf <- data.frame(wald = round(zwtest, 6), pval = round(zpval, 6))
    cat("\nWald Tests: Zero Equality\n\n")
    attr(zdf, "names") <- c("Wald Statistic", "P-Value")
    attr(zdf, "row.names") <- znam
    oldClass(zdf) <- "data.frame"
  }
  res <- list(Contrasts = cdf, Zero = zdf)
  invisible(res)
}

wald.test <- function(object, ...) {
  UseMethod("wald.test")
}
