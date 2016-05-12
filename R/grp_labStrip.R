#This function ingests group labels, colours, and variable type in a data.table
#Returns a heading strip with the appropriate aesthetics
grp_labStrip <- function(dt, tsize, grplab=T){

                  #Groups
                  grps <- dt[, group]

                  #Make top strips for each plot
                  tlist <- lapply(X=grps, FUN=function(grp){

                            #Isolate i'th group parameters
                            irow <- dt[group==grp]

                            #Get strip color
                            tcol <- irow[, col]

                            #Get label
                            if(grplab){lab <- irow[, group]}else{lab <- irow[, vartype]}

                            #Make labeller plot
                            dat <- data.frame(x=1, y=1, lab=lab)
                            gg_lab <-
                            ggplot(data=dat, aes(x=x, y=y)) +
                                      facet_wrap(~lab) + geom_blank() +

                                      theme(
                                        plot.margin=pmargin,
                                        plot.background=element_blank(),
                                        panel.background=element_blank(),
                                        axis.text.y=element_blank(),
                                        axis.ticks.y=element_blank(),
                                        axis.title.y=element_blank(),
                                        strip.text=element_text(size=tsize,
                                                                colour=ifelse(grplab, 'grey99', tcol)),
                                        strip.background=eval(parse(text=ifelse(grplab,
                                                                                "element_rect(fill=tcol, colour=tcol)",
                                                                                "element_rect(fill='transparent', colour='transparent')")))
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
