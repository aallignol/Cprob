plot.cpfpo <- function(x, layout, index, conf.int = TRUE, level = 0.95,
                       odds=TRUE, intercept=  TRUE, ylab, xlab,
                       title, lty = c(1,3,3), col = c(1,1,1),
                       ...) {
    if (!inherits(x,"cpfpo"))
        stop ("'x' must be of class 'cpfpo'")
    if (missing(ylab)) {
        if (odds==TRUE)
            ylab <- "odds-ratio"
        else
            ylab <- "log-odds-ratio"
    }
    if (missing(xlab))
        xlab <- "time"
    if (missing(title))
        title <- colnames(x$alpha)
    if (missing(index)) {
        if (intercept==FALSE)
            nc <- 2:ncol(x$alpha)
        else nc <- 1:ncol(x$alpha)
    }
    else {
        if (sum(index <= 0) > 1 | max(index) > ncol(x$alpha))
            stop("'index' is not good")
        nc <- index
    }
    if (missing(layout))
        layout <- c(2, length(nc) %/% 2 + length(nc) %% 2)
    op <- par(mfrow=layout)
    for (i in nc) {
        tis <- x$tis
        coef <- x$alpha[,i]
        cimoins <- coef-qnorm(level + (1-level)/2)*x$valpha[,i]
        ciplus <- coef+qnorm(level + (1-level)/2)*x$valpha[,i]
        if (odds) {
            coef <- exp(coef)
            cimoins <- exp(cimoins)
            ciplus <- exp(ciplus)
        }
        if (conf.int) {
            ylim <- c(min(cimoins),max(ciplus))
            xlim <- c(tis[1],tis[length(tis)])
            plot(tis,coef,xlim=xlim,ylim=ylim,type="s",
                 main=title[i],ylab=ylab,xlab=xlab,
                 col=col[1],lty=lty[1],...)
            lines(tis,cimoins,type="s",lty=lty[2],col=col[2],...)
            lines(tis,ciplus,type="s",lty=lty[3],col=col[3],...)
        }
        else {
            ylim <- c(min(coef),max(coef))
            xlim <- c(tis[1],tis[length(tis)])
            plot(tis,coef,xlim=xlim,ylim=ylim,type="s",
                 main=title[i],ylab=ylab,xlab=xlab,
                 col=col[1],lty=lty[1],...)
        }
    }
    par(op)
}
