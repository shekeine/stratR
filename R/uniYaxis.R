#Build universal y axis facet
#Returns grob object with universal age axis

uniYaxis <- function(tym_brks, st_ht){

                                #Observation time points
                                tlevs <- dat_var[[1]][, agebp]

                                #Make dummy data
                                dat_i <- data.frame(x=tlevs, vals=1:length(tlevs), var_ids='uni_y')

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
                                              plot.margin=pmargin,
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
                                              axis.ticks.length=unit(1.5, 'mm'),
                                              axis.text.y=element_text(size=axs_tsize)
                                            )

                                #Grob ggplot object and adjust strip height
                                gg_axis <- arrangeGrob(gg_axis)
                                gg_axis[["grobs"]][[1]][['heights']][gg_axis[["grobs"]][[1]]$layout$name=="strip_t-1"] <- st_ht

                                return(gg_axis)
}
