#Call helper functions
stratPlot <- function(dat=dat,
                      grpcol='group',
                      tymcol='agebp',
                      varcol='variable',
                      valcol='value',
                      varord=c('alph', 'med')[1],

                      grp_ord=unique(dat[, get(grpcol)]),
                      grp_geom=c('area', 'line')[1],
                      grp_vartype='',
                      const_grps=NULL,
                      grp_colours=brewer.pal(n=length(grp_ord), name='Dark2'),

                      xlim1=NULL,
                      xbline=NULL,
                      xbrkint=5,
                      ybrkint=500,

                      axs_tsize=20,
                      var_tsize=22,
                      grp_tsize=25,
                      ptt_tsize=25,

                      exag_by=5,

                      plab='',
                      ylab='Time variable',
                      pmargin=unit(c(0.05, 0, 0.1, 0), 'cm')
                      ){

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
              setkey(dat)

            #Make plotting parameters

              #Global parameters
                #Y axis expansion factor
                expF <- c(0, 0)

                #Time axis breaks and labels
                tym_brks <- round(seq(from=0-ybrkint,
                                      to=round_any(x=max(dat[, agebp], na.rm=T),
                                                   ybrkint, ceiling),
                                      by=ybrkint), -1)
                tym_labs <- tym_brks + rep(c(NA, 0), length.out=length(tym_brks))
                tym_labs[is.na(tym_labs)] <- ''



                #xlim1: left extent of plots in each group. This is actually ylim1 since we'll coord_flip()
                if(!length(xlim1)){
                  dt1 <- dat[, min(value, na.rm=T), .(group)][group%in%grp_ord, ]
                  dt1[, xbrkint:=xbrkint] ; setnames(dt1, 'V1', 'gmin')
                  ylim1 <- dt1[, round_any(gmin, xbrkint, floor)]
                }else{ylim1 <- xlim1}

                #Calculate height needed for all facet titles
                dt_strip <- data.table(vnames=unique(dat[, variable]))
                gt_strip <- arrangeGrob(ggplot(data=dt_strip) + facet_wrap(~vnames, ncol=nrow(dt_strip)) + geom_blank() +
                                          theme(strip.text=element_text(size=20, angle=45)))
                strip_height   <- as.numeric(gt_strip[['grobs']][[1]][['heights']][3])

              #Variable-wise parameters
                #Make Ctrl table with each variable's maximum rate of occurence
                #determines value labels and plot extent along the value axis
                dat_ctrl <- dat[, max(value, na.rm=T), by=c('group', 'variable')]
                setnames(dat_ctrl, 'V1', 'maxv')

                #Calculate median
                #Determines relative plot sizes and ordering by magnitude
                dt1 <- dat[, median(value, na.rm=T), by=c('group', 'variable')]
                setnames(dt1, 'V1', 'med')
                dat_ctrl <- merge(dat_ctrl, dt1, by=c('group', 'variable'))

                #xbline: baseline position along x. This is actually a y coordinate with coord_flip()
                #If undefined, compute it from each variable's median
                #If defined merge with dat_ctrl to match with respective variables
                if(!length(xbline)){
                  dt1 <- dat[, median(value, na.rm=T), .(variable)]
                  setnames(dt1, 'V1', 'xbline')
                  dat_ctrl <- merge(dat_ctrl, dt1, by='variable')
                }else{
                  dat_ctrl <- merge(dat_ctrl, data.table(group=grp_ord, xbline), by='group')
                }

                #Add group parameters
                dat_ctrl <- merge(dat_ctrl,
                                  data.table(group=grp_ord, col=grp_colours,
                                             vartype=grp_vartype, grp_geom, ylim1, xbrkint),
                                  by='group')

                #Define group levels: determines ordering of groups
                dat_ctrl[, group:=factor(dat_ctrl$group, levels=grp_ord, ordered=T)]

                #Order variables by group, then by median or alphabetically depending on varord arg
                ordby <- ifelse(varord[1]=='alph', 'variable', 'med')
                dir   <- ifelse(ordby=='variable', 1, -1)
                setorderv(x=dat_ctrl, cols=c('group', ordby), order=c(1, dir))

                #Calculate relative sizes of variable plots (cm's) based on relative importance (medians)

                  #Widest plot will have width of 5cm
                  maxw  <- 5

                  #Plots of variables in const_grps will have width of (5/2)cm
                  constw <- 5/2

                  #Right extent of each a variable's plot (ylim[2]) will be its maxv-
                  #rounded up to the nearest xbrkint
                  dat_ctrl[, ylim2:=round_any(x=maxv, xbrkint, f=ceiling)]

                  #Width of each variable's facet is maxw factored by that variable's med/global med
                  dat_ctrl[, pwidth:=(med/max(dat_ctrl[, med]))*maxw]

                  #Set plot width of unscaled graphs to half the maximum plot size
                  dat_ctrl[group%in%const_grps, pwidth:=constw]

                  #Unscaled plots should all have the same ylim2==group-wise max
                  dat_ctrl[group%in%const_grps, ylim2:=max(ylim2), .(group)]

                  #Recalculate widths of plots that are too narrow to display variable label or plot extent

                    #For each group, get smallest pwidth that is above 0.5cm thresh-hold
                    dt1 <- dat_ctrl[pwidth>=0.5, min(pwidth), .(group)]
                    setnames(dt1, 'V1', 'pwidth2')
                    dat_ctrl <- merge(dat_ctrl, dt1, by='group', all.x=T)

                    #Get corresponding ylim2
                    dt1 <- unique(dat_ctrl[pwidth==pwidth2])[, list(group, ylim2)]
                    setnames(dt1, 'ylim2', 'ylim2_2')
                    dat_ctrl <- merge(dat_ctrl, dt1, by='group', all.x=T)

                    #Update pwidths and ylim2 of plots with pwidth<thresh-hold of 0.5
                    #Note exclusion of const_grps
                    dat_ctrl[pwidth<0.5 & !variable%in%const_grps,
                             c('ylim2', 'pwidth'):=list(ylim2_2, pwidth2)]
                    dat_ctrl[, c('ylim2_2', 'pwidth2'):=NULL]

            #Split input data.table into variable-wise list
            vlist <- lapply(X=split(1:nrow(dat),
                                    f=as.character(dat[, variable])),
                            FUN=function(x)dat[x])

            #Graph data (variable-wise)
            gglist <- stratPlot_varlist(var_list=vlist)
            glab_ht <- gglist$glab_ht
            gglist  <- gglist$grob_grp

            #Add universal y axis
             #Determine strip text height of y axis plot, ==sum of heights of group & strip.heights
              st_ht <- unit(sum(as.numeric(gglist$grobs[[2]]$grobs[[1]]$heights), strip_height), 'cm')

              #Make y axis grob
              gg_yax <- uniYaxis(tym_brks, st_ht, glab_ht)

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

             #Add spacer row at the top for some nice margin
             gg_grob <- gtable_add_rows(x=gg_grob, height=unit(0.2, 'cm'), pos=0)

             #Add spacer row at the bottom for some nice margin
             gg_grob <- gtable_add_rows(x=gg_grob, height=unit(0.2, 'cm'), pos=3)

             return(gg_grob)
}
