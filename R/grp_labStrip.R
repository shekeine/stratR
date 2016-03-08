#This function takes group labels and their colours and returns a heading strip with the appropriate aesthetics
grp_labStrip <- function(dt_col, labcol, tsize, usecol=T){

                  #Labels
                  xlabs <- unname(unlist(dt_col[, get('labcol'), with=F]))

                  #Make top strips for each plot
                  tlist <- lapply(X=xlabs, FUN=function(lab){

                            #Get strip color
                            tcol <- dt_col[dt_col[, get('labcol'), with=F][[1]]==lab, col]

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
                                                                colour=ifelse(usecol, 'black', tcol)),
                                        strip.background=eval(parse(text=ifelse(usecol, "element_rect(fill=tcol, colour=tcol)", "element_blank()")))
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
