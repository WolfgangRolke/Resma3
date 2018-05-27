mannwhitney <-
function(x,y) wilcox.test(x~factor(y))
