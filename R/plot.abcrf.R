plot.abcrf <- function(x, sumsta=NULL, n.var=20, main="", ...)
{
	old.par <- par(no.readonly = TRUE)
	if (length(x$model.rf$importance)<20) n.var <- length(x$model.rf$importance)
	modindex <- x$model.rf$y
 	if (x$lda && !is.null(sumsta)) {
		par(mfrow=c(1,2))
		varImpPlot(x$model.rf, n.var=n.var, main=main, ...)
		projections <- predict(x$model.lda, sumsta)$x
		nmod <- nlevels(modindex)
		coloris <- rainbow(nmod)
		colo <- coloris[modindex]
    if (nmod > 2) {
      plot(projections, col=colo, pch=3)
      legend("topleft", legend = as.character(levels(modindex)), col = coloris, 
             pch = 15, bty = "o", pt.cex = 2, cex = .8, horiz = TRUE, 
             inset = c(.01, .01), title = "Models", bg = "white")
    } else {
      l1 <- levels(modindex)[1]
      l2 <- levels(modindex)[2]
      d1 <- density(projections[modindex == l1, 1])
      d2 <- density(projections[modindex == l2, 1])
      coloris <- c("blue", "orange")
      xrange <- range(c(d1$x, d2$x))
      yrange <- c(0, 1.2*max(c(d1$y, d2$y)) )
      plot(d1, xlim = xrange, ylim = yrange,
           col=coloris[1], main="", xlab="")
      lines(d2, col=coloris[2])
      legend("topleft", legend = as.character(levels(modindex)), col = coloris, 
              cex = .8, horiz = TRUE, lty=1, bty="o",
             inset = c(.01, .01), title = "Models", bg = "white")
    }
	} else {
		varImpPlot(x$model.rf, n.var=n.var, main=main, ...)
	}

	par(old.par)
}