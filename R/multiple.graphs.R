multiple.graphs <-
function (plt1, plt2, plt3, plt4, Horizontal = TRUE, titles) 
{
     if(!missing(titles)) {
          plt1<-plt1+ggtitle(titles[1])
          plt2<-plt2+ggtitle(titles[2])
          if(!missing(plt3))          
               plt3<-plt3+ggtitle(titles[3])
          if(!missing(plt4))               
               plt4<-plt4+ggtitle(titles[4])          
     }     
     if(missing(plt3)) {
        if(Horizontal)
          pushViewport(viewport(layout = grid.layout(1, 2)))
        else
          pushViewport(viewport(layout = grid.layout(2, 1)))  
     }     
     else
        pushViewport(viewport(layout = grid.layout(2, 2)))     
     print(plt1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
     if(Horizontal)
        print(plt2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
     else
        print(plt2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))   
     if(!missing(plt3))
        print(plt3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1)) 
     if(!missing(plt4))
        print(plt4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)) 
    
}
