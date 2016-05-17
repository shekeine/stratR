#Function ingests dat_i, a data.table of the i'th variable
#Returns ggplot object (statigraphic plot) of that variable

stratPlot_var <- function(dat_i){

                                #Check if this is the last variable (determines whether right plot border will be extended)
                                isLast <- as.character(dat_i[, variable][1])==last(as.character(dat_ctrl[, variable]))

                                #Make plotting parameters for variable i
                                  #Group colour
                                  gcol <- dat_ctrl[group==dat_i[, group][1], col][1]

                                  #Group geom
                                  isArea <- dat_ctrl[group==dat_i[, group][1], grp_geom][1]=='area'

                                  #Get value axis limits
                                  ylims1 <- dat_ctrl[group==dat_i[, group][1], ylim1][1]
                                  ylims2 <- as.numeric(dat_ctrl[variable==as.character(dat_i[, variable][1])][, list(ylim2)])

                                  #Get baseline position
                                  grp_bline <- dat_ctrl[group==dat_i[, group][1] &
                                                       variable==as.character(dat_i[, variable][1]), grp_bline]

                                  #Subset table to make line segments at age samples
                                  seg_dt <- data.table(x1=unique(dat[, agebp]),
                                                       x2=unique(dat[, agebp]), y1=ylims1, y2=ylims2)

                                  #Value axis breaks

                                    #Extract current group's value break interval
                                    xbrkint <- dat_ctrl[group==dat_i[, group][1], xbrkint][1]

                                    #Make value breaks
                                    #If break interval is less than ylim2, vbrks will be seq(ylims1, ylim2, xbrkint)
                                    #If bigger, it will be just ylims1 and ylims2
                                    if(xbrkint < ylims2){
                                      vbrks   <- seq(ylims1, ylims2, xbrkint)[seq(ylims1, ylims2, xbrkint) <= ylims2]
                                    }else if(xbrkint >= ylims2){vbrks   <- c(ylims1, ylims2)}

                                  #Value axis labels, Label every other break, beginning with the 2nd one
                                  vlabs <- vbrks

                                  #Label only first and every other break
                                  vlabs <- rep(c(0, NA), length.out=length(vbrks)) + vbrks
                                  vlabs[is.na(vlabs)]  <- ''

                                  #If there are more than 2 vbrks, remove last vlab
                                  if(length(vbrks)>2){vlabs[length(vlabs)] <- ''}

                                  #If there are only 2 breaks, label both: DELETE
                                  #if(length(vbrks)==2){vlabs <- vbrks}

                                  #If there are 3 breaks,force labelling of 2nd
                                  if(length(vbrks)==3){vlabs[2] <- vbrks[2]}

                                gg_i <- ggplot(data=dat_i, aes(x=agebp, y=value)) + facet_wrap(~variable) +
                                        coord_flip(xlim=range(tym_brks), ylim=c(ylims1, ylims2)) + theme_bw()+

                                        scale_x_reverse(breaks=tym_brks, labels=tym_labs, expand=c(0, 0)) +
                                        scale_y_continuous(breaks=vbrks, labels=vlabs, expand=c(0, 0)) +

                                        #Main plot geometry
                                        eval(parse(text=ifelse(isArea,
                                                               "geom_area(fill=gcol, colour=gcol, size=0.3)",
                                                               "geom_line(colour=gcol, size=0.3, aes(group=1))"))) +

                                        #Secondary (exaggerated) plot geometry
                                        eval(parse(text=ifelse(isArea,
                                                               "geom_area(data=dat_i, aes(x=agebp, y=value*exag_by),
                                                                          fill=gcol, colour='grey90', alpha=0.25, size=0.3)",
                                                               "geom_blank()"))) +

                                        #Line segments at sample positions
                                        geom_segment(data=seg_dt, aes(x=x1, xend=x2,
                                                                      y=y1, yend=y2), size=0.1, colour='grey80') +

                                        #X axis line
                                        geom_segment(aes(x=max(tym_brks), xend=max(tym_brks),
                                                         y=ylims1, yend=ylims2), size=0.4, colour=gcol)+

                                       #Diagnostic red lines:Should line up with y axis breaks to the left if all subplots have
                                       #been scaled properly

#                                         geom_segment(data=data.frame(x=tym_brks, xend=tym_brks, y=ylims1, yend=ylims2),
#                                                      aes(x=x, xend=xend, y=y, yend=yend), size=0.1, colour='red') +

                                        #Top plot border
                                        geom_segment(aes(x=min(tym_brks), xend=min(tym_brks),
                                                         y=ylims1, yend=ylims2), colour=gcol) +

                                        #Add right border if this is the last plot
                                        eval(parse(text=ifelse(isLast,
                                                                     "geom_segment(aes(x=min(tym_brks),
                                                                      xend=max(tym_brks), y=ylims2, yend=ylims2), size=0.5, col=gcol)",
                                                               "geom_blank()"))) +

                                        geom_point(colour="grey40", size=0.2) +

                                        #Baseline
                                        geom_abline(intercept=grp_bline, slope=0, size=0.2, colour="red", linetype=2, na.rm=T) +

                                        theme(
                                          plot.margin=pmargin,
                                          plot.background=element_rect(fill='transparent', colour='transparent'),

                                          #Diagnostic plot border: change fill or colour to see subplot layout
                                          panel.border=element_rect(fill='transparent', colour='transparent'),
                                          panel.grid=element_blank(),
                                          panel.background=element_blank(),

                                          strip.text=element_text(colour=gcol, size=var_tsize, angle=45, vjust=0, hjust=0),
                                          strip.background=element_blank(),

                                          axis.ticks.length=unit(len_tick, 'cm'),
                                          axis.ticks.x=element_line(size=0.2, colour=gcol),
                                          axis.ticks.y=element_blank(),

                                          axis.line=element_line(size=0.1),
                                          axis.line.y=element_blank(),

                                          axis.title.x=element_blank(),
                                          axis.title.y=element_blank(),

                                          axis.text.x=element_text(size=axs_tsize, colour=gcol),
                                          axis.text.y=element_blank()
                                        )
                              return(gg_i)
}
