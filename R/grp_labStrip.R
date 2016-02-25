#This function takes group labels and their colours and returns a heading strip with the appropriate aesthetics
grp_labStrip <- function(dt_col){
    
                  #Make top strips for each plot
                  tlist <- lapply(X=dt_col[, group], FUN=function(grp){

                            #Get strip color
                            tcol <- dt_col[group==grp, col]
                            
                            #Make labeller plot
                            gg_lab <- 
                            ggplot(data=data.frame(x=1, y=1, grp=grp), aes(x=x, y=y)) + 
                                      facet_wrap(~grp) + geom_blank() +
                              
                                      theme(
                                        plot.margin=pmargin,
                                        plot.background=element_blank(),
                                        panel.background=element_blank(),
                                        axis.text.y=element_blank(),
                                        axis.ticks.y=element_blank(),
                                        axis.title.y=element_blank(),
                                        strip.text=element_text(size=grp_tsize),
                                        strip.background=element_rect(fill=tcol, colour=tcol)
                                      )
                            lab_grob <- arrangeGrob(gg_lab)$grobs[[1]][3,]
                            return(lab_grob)
                  })

                  #Relative group widths
                  grpW <- dat_ctrl[, sum(pwidth), by='group'][, V1]
                  
                  #Arrange strips
                  tstrip <- do.call(arrangeGrob, c(tlist, list(widths=lapply(grpW, unit, "cm"))))
                  return(tstrip)
                  }