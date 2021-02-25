# lh_plot single test

lh_plot_single.test<-function(lh.obj,marker){
  lhb.table<-lh.obj$behavior.table
  selected.table<- lhb.table[str_which(names(lh.obj$behavior.table),marker)]
  
  mice.plot<-map(names(selected.table),function(given_name){
    given_mice<-selected.table[[which(names(selected.table)==given_name)]]
    curr.plot<-ggplot(data=given_mice,aes(trail.number,latency))+
      geom_point(size=3,color=given_mice$action)+geom_line(size=0.1)+theme_bw()+
      ylab('')+xlab('')+ggtitle(given_name)+ylim(-0.1,15)+xlim(0,30)
      theme(title=element_text(size=8))
    return(curr.plot)
  })
  
  mice.grid<-plot_grid(plotlist = mice.plot,ncol=4)
  plot_grid(plotlist = mice.plot,ncol=4)
  return(mice.grid)
}