ggMarginal <-
function (p, data, x, y, type = c("density", "histogram", "boxplot"), 
    margins = c("both", "x", "y"), size = 5, ..., xparams, yparams) 
{
    type <- match.arg(type)
    margins <- match.arg(margins)
    extraParams <- list(...)
    if (is.null(extraParams[["colour"]]) && is.null(extraParams[["color"]]) && 
        is.null(extraParams[["col"]])) {
        extraParams[["colour"]] <- "black"
    }
    if (is.null(extraParams[["fill"]])) {
        extraParams[["fill"]] <- "grey"
    }
    if (type == "density") {
        extraParams[["fill"]] <- NULL
    }
    if (missing(xparams)) {
        xparams <- list()
    }
    else {
        xparams <- as.list(xparams)
    }
    if (missing(yparams)) {
        yparams <- list()
    }
    else {
        yparams <- as.list(yparams)
    }
    if (missing(p)) {
        if (missing(data) || missing(x) || missing(y)) {
            stop("`data`, `x`, and `y` must be provided if `p` is not provided", 
                call. = FALSE)
        }
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x, y)) + 
            ggplot2::geom_point()
        x <- as.symbol(x)
        y <- as.symbol(y)
    }
    else {
        if (missing(data)) {
            if (methods::is(p$data, "waiver")) {
                stop("`data` must be provided if it is not part of the main ggplot object", 
                  call. = FALSE)
            }
            data <- p$data
        }
        if (length(p$mapping) == 0) 
            p$mapping <- p$layers[[1]]$mapping
        if (margins != "y" && missing(x)) {
            if (is.null(p$mapping$x)) {
                stop("`x` must be provided if it is not an aesthetic of the main ggplot object", 
                  call. = FALSE)
            }
            x <- p$mapping$x
        }
        if (margins != "x" && missing(y)) {
            if (is.null(p$mapping$y)) {
                stop("`y` must be provided if it is not an aesthetic of the main ggplot object", 
                  call. = FALSE)
            }
            y <- p$mapping$y
        }
    }
    if (!missing(x)) 
        xvar <- x
    if (!missing(y)) 
        yvar <- y
    p <- p + ggplot2::theme(plot.margin = grid::unit(c(0, 0, 
        0, 0), "null"))
    pb <- ggplot2::ggplot_build(p)
    hasTitle <- (!is.null(pb$plot$labels$title))
    if (hasTitle) {
        title <- grid::textGrob(pb$plot$labels$title, gp = grid::gpar(col = pb$plot$theme$plot.title$colour, 
            fontsize = 16))
        p$labels$title <- NULL
    }
    if (margins != "y") {
        top <- marginPlot(margin = "x", type = type, xvar = xvar, 
            yvar = yvar, xparams = xparams, yparams = yparams, 
            pb = pb, data = data, extraParams = extraParams)
        top <- addMainTheme(marginal = top, margin = "x", p = p)
        top <- top + ggplot2::ylab(p$labels$y) + getScale(margin = "x", 
            type = type, pb = pb)
        pbTop <- ggplot2::ggplot_build(top)
        ylabels <- pb$panel$ranges[[1]]$y.labels
        ylabel <- ylabels[which.max(nchar(ylabels))]
        if (type == "boxplot") {
            top <- top + ggplot2::scale_x_continuous(breaks = mean(getLimits(pbTop, 
                "x")), labels = ylabel)
        }
        else {
            top <- top + ggplot2::scale_y_continuous(breaks = mean(getLimits(pbTop, 
                "y")), labels = ylabel)
        }
    }
    if (margins != "x") {
        right <- marginPlot(margin = "y", type = type, xvar = xvar, 
            yvar = yvar, xparams = xparams, yparams = yparams, 
            pb = pb, data = data, extraParams = extraParams)
        right <- addMainTheme(marginal = right, margin = "y", 
            p = p)
        right <- right + ggplot2::ylab(p$labels$x) + getScale(margin = "y", 
            type = type, pb = pb)
    }
    pGrob <- ggplot2::ggplotGrob(p)
    suppressMessages({
        if (margins == "both") {
            ggxtra_tmp <- addTopMargPlot(ggMargGrob = pGrob, 
                top = top, size = size)
            ggxtra_nottl <- addRightMargPlot(ggMargGrob = ggxtra_tmp, 
                right = right, size = size)
        }
        else if (margins == "x") {
            ggxtra_tmp <- gtable::gtable_add_padding(x = pGrob, 
                grid::unit(c(0, 0.5, 0, 0), "lines"))
            ggxtra_nottl <- addTopMargPlot(ggMargGrob = ggxtra_tmp, 
                top = top, size = size)
        }
        else if (margins == "y") {
            ggxtra_tmp <- gtable::gtable_add_padding(x = pGrob, 
                grid::unit(c(0.5, 0, 0, 0), "lines"))
            ggxtra_nottl <- addRightMargPlot(ggMargGrob = ggxtra_tmp, 
                right = right, size = size)
        }
    })
    if (hasTitle) {
        titleH <- grid::grobHeight(title)
        gt_t <- gtable::gtable_add_rows(x = ggxtra_nottl, heights = titleH, 
            pos = 0)
        maxR <- max(gt_t$layout$r)
        ggExtraPlot <- gtable::gtable_add_grob(x = gt_t, grobs = title, 
            t = 1, b = 1, l = 1, r = maxR, z = Inf, clip = "on", 
            name = "plotTitle")
    }
    else {
        ggExtraPlot <- ggxtra_nottl
    }
    class(ggExtraPlot) <- c("ggExtraPlot", class(ggExtraPlot))
    ggExtraPlot
}
