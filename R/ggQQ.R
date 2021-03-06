ggQQ <-
function(LM) # argument: a linear model
    {
        y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
        x <- qnorm(c(0.25, 0.75))
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
        p <- ggplot(LM, aes(sample=.resid)) +
            stat_qq(alpha = 0.5) +
            geom_abline(slope = slope, intercept = int, color="blue")     
        return(p)
    }
