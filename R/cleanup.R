cleanup <-
function () 
{
      oldstuff<-c("f","f1","n","m","a","A","b","i","j","plt","plt1","plt2","x","xx","y","z")
      rm(list=oldstuff,envir = environment(sv))
}
