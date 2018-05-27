scf <-
function(f, Run.it = TRUE) { 
    source(paste0(getwd(),"/Functions/", f, ".R")) 
    if( Run.it ) get(f)()
}
