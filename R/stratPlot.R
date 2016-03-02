#Call helper functions
stratPlot <- function(dat=dat, grp_ord=c('Trees', 'Shrub', 'Herb'),
                      tymcol='agebp',
                      varcol='variable',
                      valcol='value',
                      grpcol='group',
                      gcols=brewer.pal(n=length(unique(dat[, group])), name='Dark2'),

                      xbrkint=5,
                      ybrkint=500,

                      axs_tsize=20,
                      var_tsize=22,
                      grp_tsize=25,
                      ptt_tsize=25,

                      plab='',
                      xlab='',
                      ylab='Time variable',
                      pmargin=unit(c(0.05, 0, 0.1, 0), 'cm')){

            #Set helper functions environments to dynamically scope
            environment(stratPlot_varlist) <- environment()
            environment(stratPlot_var) <- environment()
            environment(grp_labStrip) <- environment()
            environment(uniYaxis) <- environment()

            #Prepare input data
              #Set column names
              setnames(x=dat, old=c(tymcol, varcol, valcol, grpcol),
                       new=c('agebp', 'variable', 'value', 'group'))

              #Remove variables that do not occur at least once
              dat[, ntNA:=sum(!is.na(value)), by=variable]
              dat <- dat[ntNA>0]
              dat[, ntNA:=NULL]

              #Sort input data by seting DT key
              setkey(dat)

            #Make plotting parameters
              #Global parameters
                #Y axis expansion factor
                expF <- c(0, 0)

                #Time breaks (Age/time/depth) and labels
                tym_brks <- round(seq(from=0-ybrkint, to=roundUp(max(dat[, agebp], na.rm=T), ybrkint)+ybrkint, by=ybrkint), -1)
                tym_labs <- tym_brks + rep(c(NA, 0), length.out=length(tym_brks))
                tym_labs[is.na(tym_labs)] <- ''

                #Define range within which variable values, hence axis breaks and labels can possibly occur
                labsr <- seq(0, 100, 10)

                #Colours to group higher levels by (including y axis)
                dt_col <- data.table(group=grp_ord, col=gcols)

                #Fun to make grp_labs
                #source('fun/grp_labStrip.R', local=T)

                #Get height needed for all facet titles
                dt_strip <- data.table(vnames=unique(dat[, variable]))
                gt_strip <- arrangeGrob(ggplot(data=dt_strip) + facet_wrap(~vnames, ncol=nrow(dt_strip)) + geom_blank() +
                                          theme(strip.text=element_text(size=20, angle=45)))
                strip_height   <- as.numeric(gt_strip[['grobs']][[1]][['heights']][3])

              #Variable-wise parameters
                #Make Ctrl table with each variable's maximum rate of occurence
                dat_ctrl <- dat[, max(value, na.rm=T), by=c('group', 'variable')]
                setnames(dat_ctrl, 'V1', 'maxv')

                #Order by factor and maxv
                dat_ctrl[, group:=factor(dat_ctrl$group, levels=grp_ord, ordered=T)]
                dat_ctrl <- dat_ctrl[with(dat_ctrl, order(+as.numeric(group), -maxv))]

                #Determine relative sizes of variable facets (in cm) from maxv

                #Set plot width (cms) of variable with highest maxv
                maxw  <- 5

                #ylim[2]  of each variable will be maxv rounded up to the nearest 5
                dat_ctrl[, ylim1:=0]
                dat_ctrl[, ylim2:=roundUp(x=maxv,to=5)]

                #Width of each variable's facet is maxw factored by that variable's maxv/global ylim[2]
                dat_ctrl[, pwidth:=(ylim2/max(dat_ctrl[, ylim2]))*maxw]

                #Split input data into variable-wise list
                dat_var <- lapply(X=split(1:nrow(dat), f=as.character(dat[, variable])), FUN=function(x)dat[x])

                #Graph each variable (group-wise)
                gglist  <- stratPlot_varlist(dat_var)

            #Add universal y axis
             #Determine strip text height of y axis plot==sum of heights of group & strip.heights
              st_ht <- unit(sum(as.numeric(gglist$grobs[[2]]$grobs[[1]]$heights), strip_height), 'cm')

              #Make y axis grob
              #source('fun/uniYaxis.R', local=T)
              gg_yax <- uniYaxis(tym_brks, st_ht)

             #Get total width of y axis grob, less default annotation allowance (0.2), plus that of accompanying title
              axw <- sum(grobWidth(gg_yax$grobs[[1]][['grobs']][gg_yax$grobs[[1]]$layout$name=='ylab'][[1]]),
                         grobWidth(gg_yax$grobs[[1]][['grobs']][gg_yax$grobs[[1]]$layout$name=='ylab'][[1]])*0.1,
                         grobWidth(gg_yax$grobs[[1]][['grobs']]$axis_l))

             #Add y axis grob as left annotation, occupying a column with just enough width: axw
             gg_grob <- arrangeGrob(gglist, left=gg_yax, padding=axw)

            #Add plot labels (if supplied)

             #Plot label
             if(nchar(plab) > 1){
               plab_grob <- textGrob(label=plab, gp=gpar(fontsize=ptt_tsize, fontface='bold', col="grey30"))
               gg_grob <- arrangeGrob(gg_grob, top=plab_grob, padding=unit(0.5, units='cm'))
             }

             #X axis label
             if(nchar(xlab) > 1){
               xlab_grob <- textGrob(label=xlab, gp=gpar(fontsize=axs_tsize, col="grey30"))
               gg_grob   <- arrangeGrob(gg_grob, bottom=xlab_grob, padding=unit(0.5, units='cm'))
             }

             #Add spacer row at the top for some nice margin
             gg_grob <- gtable_add_rows(x=gg_grob, height=unit(0.2, 'cm'), pos=0)

             return(gg_grob)
}
