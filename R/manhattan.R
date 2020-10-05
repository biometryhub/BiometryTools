#' Manhattan plot using ggplot
#'
#' @param mlist
#' @param cross
#' @param chr.in
#' @param annotate
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
manhattan <- function(mlist, cross, chr.in = NULL, annotate = TRUE, ...) {
  nams <- names(mlist)
  outs <- lapply(mlist, function(el) {
    temp <- el$QTL$diag$oint[[1]]
    names(temp) <- gsub("Chr\\.", "", names(temp))
    temp
  })
  if (!is.null(chr.in)) {
    cross <- subset(cross, chr = chr.in)
    for (i in 1:length(outs)) {
      onam <- sapply(strsplit(names(outs[[i]]), "\\."), "[", 1)
      outs[[i]] <- outs[[i]][onam %in% chr.in]
    }
  }
  dat <- cbind.data.frame(value = unlist(outs))
  dat$nout <- sapply(outs, names)
  len <- sapply(outs, length)[1]
  dat$Name <- rep(nams, each = len)
  chr <- rep(names(cross$geno), times = nmar(cross))
  dat$chr <- factor(rep(chr, length(nams)))
  dist <- lapply(cross$geno, function(el) {
    tel <- c(100000, diff(el$map))
    names(tel)[1] <- names(el$map)[1]
    tel
  })
  dist <- cumsum(unlist(dist))
  sp <- unlist(lapply(split(dist, chr), function(el) min(el) + diff(range(el)) / 2))
  spc <- unlist(lapply(split(dist, chr), function(el) max(el) + 500000))
  dat$dist <- rep(dist, length(nams))
  dat$chr.g <- dat$chr
  levels(dat$chr.g) <- c(rep(c("g1", "g2"), 10), "g1")
  cols <- brewer.pal(3, "Set1")[1:2]
  gp <- ggplot(dat, aes(x = dist, y = value)) +
    facet_wrap(~Name, ncol = 1, scales = "free_y") +
    geom_vline(xintercept = spc, colour = "gray80") +
    geom_point(aes(colour = chr.g)) +
    scale_y_continuous(expand = c(0.02, 0), breaks = seq(0, 100, by = 10)) +
    scale_x_continuous(breaks = sp, labels = names(cross$geno), expand = c(0.02, 0)) +
    xlab("") +
    ylab("Outlier Statistic") +
    scale_color_manual(values = cols) +
    theme(legend.position = "none", axis.text = element_text(size = 10), panel.background = element_blank(), panel.border = element_rect(colour = "gray80", fill = NA, size = 1.1), panel.grid.major.y = element_line(colour = "gray90", size = 1.2), panel.grid.minor.y = element_line(colour = "gray90", size = 0.8), panel.grid.major.x = element_blank(), axis.title = element_text(size = 20), strip.text = element_text(size = 10))
  if (annotate) {
    qtl <- lapply(mlist, function(el) gsub("Chr\\.", "", el$QTL$qtl))
    qtl.dat <- cbind.data.frame(Name = rep(nams, times = sapply(qtl, length)))
    qtl.dat$nout <- unlist(qtl)
    if (!is.null(chr.in)) {
      qnam <- sapply(strsplit(as.character(qtl.dat$nout), "\\."), "[", 1)
      print(qnam)
      qtl.dat <- qtl.dat[qnam %in% chr.in, ]
    }
    print(qtl.dat)
    subexpr <- paste(dat$Name, dat$nout, sep = ":") %in% paste(qtl.dat$Name, qtl.dat$nout, sep = ":")
    ann <- subset(dat, subexpr)
    print(ann)
    gp + geom_text(data = ann, aes(label = nout), size = 5)
  }
}
