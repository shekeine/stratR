#Build universal y axis facet
#Returns grob object with universal age axis

uniYaxis <- function(tym_brks, st_ht, tstrip_h){

                                #Observation time points
                                tlevs <- unique(dat[, agebp])

                                #Make dummy data
                                dat_i <- data.frame(x=tlevs, vals=1:length(tlevs), var_ids='uni_y')

                                #Update bottom margin by adding size of bottom strip to bottom margin

                                  #New bottom margin
                                    #Get height of bottom strip with the variable type labels and update margin
                                    bs_ht <- gglist$heights[2]
                                    bmar <- convertX(x=sum(pmargin[3], bs_ht), unitTo='cm')

                                  #New top margin, for some reason, Difference betw heights of top strip with group labels
                                  #and bottom strip with vartype labels needs to be discounted:: To investigate cleaner solution
                                  diff_ht <- tstrip_h-bs_ht
                                  tmar <- convertX(x=sum(pmargin[1], diff_ht), unitTo='cm')

                                  #New top margin
                                  pmargin2 <- unit.c(tmar, pmargin[2], bmar,
                                                     unit(as.numeric(pmargin[4]), 'cm'))

                                #Build plot
                                gg_axis <- ggplot(data=dat_i, aes(y=vals, x=x)) + facet_wrap(~var_ids) +
                                            coord_flip(xlim=range(tym_brks), ylim=c(0,1)) + theme_bw() + geom_blank() +

                                            scale_x_reverse(breaks=tym_brks, labels=tym_labs, expand=c(0,0)) +
                                            scale_y_continuous(expand=c(0,0)) +

                                            labs(x=ylab) +

                                            #Diagnostic red line seg.s that should line up wiht y axis breaks to the left
#                                             geom_segment(data=data.frame(
#                                                     x=tym_brks, xend=tym_brks, y=ylims1, yend=ylims2)
#                                                   ,aes(x=x, xend=xend, y=y, yend=yend), size=0.1, colour='red') +

                                            #Universal axis line
                                            geom_segment(aes(x=min(tym_brks), xend=max(tym_brks), y=0, yend=0), size=0.7) +
                                            theme(
                                              plot.margin=pmargin2,
                                              #panel.border=element_rect(fill='white', colour='black'),
                                              panel.border=element_blank(),
                                              panel.grid=element_blank(),
                                              panel.background=element_blank(),
                                              plot.background=element_rect(fill='transparent', colour=NA),

                                              strip.text=element_text(size=var_tsize, angle=45, vjust=0, hjust=0),
                                              strip.background=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_text(size=axs_tsize, colour='grey30',
                                                                        margin=margin(r=0.25, l=0.25, unit = "cm")),
                                              axis.ticks.x=element_blank(),
                                              axis.text.x=element_text(size=axs_tsize, colour='transparent'),
                                              axis.ticks.y=element_line(size=0.2, colour='black'),
                                              axis.ticks.length=unit(len_tick, 'cm'),
                                              axis.text.y=element_text(size=axs_tsize)
                                            )

                                #Grob ggplot object and adjust strip height
                                gg_axis <- arrangeGrob(gg_axis)
                                gg_axis[["grobs"]][[1]][['heights']][gg_axis[["grobs"]][[1]]$layout$name=="strip_t-1"] <- st_ht

                                return(gg_axis)
}
