filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    # further modified by Carey McGilliard and Bridget Ferris
    # to allow multiple plots on one page
    
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    # on.exit(par(par.orig))
    # w <- (3 + mar.orig[2]) * par("csi") * 2.54
    # par(las = las)
    # mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
if (missing(plot.axes)) {
  if (axes) {
    title(main = "", xlab = "", ylab = "")
    Axis(x, side = 1)
    Axis(y, side = 2)
  }
}
else plot.axes
if (frame.plot) 
  box()
if (missing(plot.title)) 
  title(...)
else plot.title
invisible()
  }


plot.svm <-
  function(x, data, formula = NULL, fill = TRUE,
           grid = 50, slice = list(), symbolPalette = palette(),
           svSymbol = "x", dataSymbol = "o", 
           mytitle='SVM Graph',...)
  {
    if (x$type < 3) {
      if (is.null(formula) && ncol(data) == 3) {
        formula <- formula(delete.response(terms(x)))
        formula[2:3] <- formula[[2]][2:3]
      }
      if (is.null(formula))
        stop("missing formula.")
      if (fill) {
        sub <- model.frame(formula, data)
        xr <- seq(min(sub[, 2]), max(sub[, 2]), length = grid)
        yr <- seq(min(sub[, 1]), max(sub[, 1]), length = grid)
        l <- length(slice)
        if (l < ncol(data) - 3) {
          slnames <- names(slice)
          slice <- c(slice, rep(list(0), ncol(data) - 3 -
                                  l))
          names <- labels(delete.response(terms(x)))
          names(slice) <- c(slnames, names[!names %in%
                                             c(colnames(sub), slnames)])
        }
        for (i in names(which(sapply(data, is.factor))))
          if (!is.factor(slice[[i]])) {
            levs <- levels(data[[i]])
            lev <- if (is.character(slice[[i]])) slice[[i]] else levs[1]
            fac <- factor(lev, levels = levs)
            if (is.na(fac))
              stop(paste("Level", dQuote(lev), "could not be found in factor", sQuote(i)))
            slice[[i]] <- fac
          }
        
        lis <- c(list(yr), list(xr), slice)
        names(lis)[1:2] <- colnames(sub)
        new <- expand.grid(lis)[, labels(terms(x))]
        preds <- predict(x, new)
        filled.contour3(xr, yr,
                       matrix(as.numeric(preds),
                              nrow = length(xr), byrow = TRUE),
                       plot.axes = {
                         axis(1, cex.axis=0.5)
                         axis(2, cex.axis=0.5)
                         colind <- as.numeric(model.response(model.frame(x, data)))
                         dat1 <- data[-x$index,]
                         dat2 <- data[x$index,]
                         coltmp1 <- symbolPalette[colind[-x$index]]
                         coltmp2 <- symbolPalette[colind[x$index]]
                         points(formula, data = dat1, pch = dataSymbol, col = coltmp1, cex=3)
                         points(formula, data = dat2, pch = svSymbol, col = coltmp2, cex=0.8)
                         cex.axis=0.5
                       },
                       levels = 1:(length(levels(preds)) + 1),
                       key.axes = axis(4, 1:(length(levels(preds))) + 0.5,
                                       labels = levels(preds),
                                       las = 3),
                       plot.title = {par(cex.main=0.8, cex.axis=0.8);
                         title(main=mytitle,line=1,xlab=names(lis)[2], ylab=names(lis)[1], cex.lab=0.5)},
                       
                       ...)
      }
      else {
        plot(formula, data = data, type = "n", ...)
        colind <- as.numeric(model.response(model.frame(x,
                                                        data)))
        dat1 <- data[-x$index,]
        dat2 <- data[x$index,]
        coltmp1 <- symbolPalette[colind[-x$index]]
        coltmp2 <- symbolPalette[colind[x$index]]
        points(formula, data = dat1, pch = dataSymbol, col = coltmp1)
        points(formula, data = dat2, pch = svSymbol, col = coltmp2)
        invisible()
      }
    }
  }