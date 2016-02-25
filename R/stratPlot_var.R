#Function takes stratigraphic data of i'th variable
#Returns ggplot object with stratigraphic plot of i'th variable

stratPlot_var <- function(dat_i){

                                #Check if this is the last variable (determines whether right plot border is added)
                                isLast <- as.character(dat_i[, variable][1])==last(as.character(dat_ctrl[, variable]))

                                #Make plotting parameters for variable i
                                #Group colour
                                gcol <- dt_col[group==dat_i[, group][1], col]

                                #Plot limits0
                                ylims <- as.numeric(dat_ctrl[variable==as.character(dat_i[, variable][1])][, list(ylim1, ylim2)])
                                ylims1 <- ylims[1]
                                ylims2 <- ylims[2]

                                #Value axis breaks
                                vbrks <- seq(0, 100, xbrkint)[seq(0, 100, xbrkint) <= ylims[2]]

                                #Value axis labels, Label every other break, beginning with the 2nd one
                                vlabs <- vbrks

                                #Label only first and every other break
                                vlabs <- rep(c(NA, 0), length.out=length(vbrks)) + vbrks
                                vlabs[is.na(vlabs)]  <- ''

                                #If there are more than 2 vbrks, remove last vlab.
                                #If there are only two vbrks, (0 and other (ylim[2])), force labelling of last break
                                if(length(vbrks)>2){vlabs[length(vlabs)] <- ''}else
                                  if(length(vbrks)==2){vlabs[length(vlabs)] <- vbrks[2]}

                                gg_i <- ggplot(data=dat_i, aes(x=agebp, y=value)) + facet_wrap(~variable) +
                                        coord_flip(xlim=range(tym_brks), ylim=ylims) + theme_bw()+

                                        scale_x_reverse(breaks=tym_brks, labels=tym_labs, expand=c(0, 0)) +
                                        scale_y_continuous(breaks=vbrks, labels=vlabs, expand=c(0, 0)) +

                                        geom_area(fill=dt_col[group==dat_i[, group][1], col], size=0.3) +
                                        geom_segment(aes(x=agebp, xend=agebp, y=ylims1, yend=ylims2), size=0.1, colour='grey10') +

                                       #Diagnostic red line seg.s that should line up wiht y axis breaks to the left
#                                         geom_segment(data=data.frame(x=tym_brks, xend=tym_brks, y=ylims1, yend=ylims2),
#                                                      aes(x=x, xend=xend, y=y, yend=yend), size=0.1, colour='red') +

                                        #Top plot border
                                        geom_segment(aes(x=min(tym_brks), xend=min(tym_brks), y=ylims1, yend=ylims2), colour=gcol) +

                                        eval(parse(text=ifelse(isLast,
                                                                     "geom_segment(aes(x=min(tym_brks),
                                                                      xend=max(tym_brks), y=ylims2, yend=ylims2), size=0.7, col=gcol)",
                                                               "geom_blank()"))) +

                                        geom_point(colour="black", size=0.3) +
                                        geom_abline(intercept=0, slope=0, size=0.3, colour="red", linetype=2) +

                                        theme(
                                          plot.margin=pmargin,

                                          #Diagnostic plot border that when changed to element_rect shows where everything is
                                          panel.border=element_blank(),
                                          panel.grid=element_blank(),
                                          panel.background=element_blank(),

                                          strip.text=element_text(colour=gcol, size=var_tsize, angle=45, vjust=0, hjust=0),
                                          strip.background=element_blank(),

                                          axis.ticks.length=unit(1.5, 'mm'),
                                          axis.ticks.x=element_line(size=0.2),
                                          axis.ticks.y=element_blank(),

                                          axis.line=element_line(size=0.1),
                                          axis.line.y=element_blank(),

                                          axis.title.x=element_blank(),
                                          axis.title.y=element_blank(),

                                          axis.text.x=element_text(size=axs_tsize),
                                          axis.text.y=element_blank()
                                        )
                              return(gg_i)
}
