grid_arrange_shared_legend <- function(...) {

    # copied from https://stackoverflow.com/a/28594060
        
    library(ggplot2)
    library(gridExtra)
    library(grid)
    
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
        do.call(arrangeGrob, lapply(plots, function(x)
            x + theme(legend.position="none"))),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight))
}

# dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
# p1 <- qplot(carat, price, data=dsamp, colour=clarity)
# p2 <- qplot(cut, price, data=dsamp, colour=clarity)
# p3 <- qplot(color, price, data=dsamp, colour=clarity)
# p4 <- qplot(depth, price, data=dsamp, colour=clarity)
# 
# 
# grid_arrange_shared_legend(actplot, p1p2plot)
