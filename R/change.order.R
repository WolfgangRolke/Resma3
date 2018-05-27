change.order <-
function (z, new.order) 
{
   if (is.numeric(new.order)) 
        mylevels <- unique(z)[new.order]
   else mylevels <- new.order
   factor(z, levels = mylevels, ordered = TRUE)
}
