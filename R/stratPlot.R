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

                      eqAxes=T,
                      xbline=NULL,
                      xbrkint=NA,
                      ybrkint=NA,

                      axs_tsize=20,
                      var_tsize=15,
                      grp_tsize=25,
                      ptt_tsize=25,

                      exag_by=5,

                      plab='',
                      ylab='Time variable',
                      pmargin=unit(c(0.05, 0, 0.1, 0), 'cm'),
                      maxw=5
                      ){

            #Set helper functions' environments to dynamically scope
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

                #Heights of spacer rows (cms)
                spacer_h <- 0.2

                #Length of axis tick lengths (cm): also left margin
                len_tick <- 0.14

                #Make y axis breaks, Note: computed if not supplied
                if(is.na(ybrkint)){
                  #Automated break interval
                  ybrkint <- diff(pretty(range(dat[, agebp]), n=10))[1]/2
                }
                tym_brks <- round(seq(from=round_any(x=min(dat[, agebp], na.rm=T),
                                                     ybrkint, floor),
                                      to=round_any(x=max(dat[, agebp], na.rm=T),
                                                   ybrkint, ceiling),
                                      by=ybrkint)
                                  )
                #Ensure some margin betw plot geometry and axes
                if((abs(last(tym_brks) - max(dat[, agebp]))) < (0.1*ybrkint)){
                  tym_brks[length(tym_brks)] <- last(tym_brks) + ybrkint
                }
                if((abs(tym_brks[1] - min(dat[, agebp]))) < (0.1*ybrkint)){
                  tym_brks[1] <- tym_brks[1] + ybrkint
                  }

                #Make y axis labels
                tym_labs <- tym_brks + rep(c(0, NA), length.out=length(tym_brks))
                tym_labs[is.na(tym_labs)] <- ''

              #Make ylim1 & ylim2. All vars in a group get same ylim2 2B comparable in absolute terms
              ylim1 <- dat[, min(pretty(range(value)), na.rm=T), .(group)][group%in%grp_ord, V1]
              ylim2 <- dat[, max(pretty(range(value)), na.rm=T), .(group)][group%in%grp_ord, V1]

              #Make xbrkint: for automatic x axis breaks
              xbrkint_auto <- dat[, diff(pretty(range(value)))[1], .(group)][group%in%grp_ord, V1]

              #Variable-wise parameters
                #Make Ctrl table with each variable's maximum rate of occurence
                #determines value labels and plot extent along the value axis
                dat_ctrl <- dat[, max(value, na.rm=T), by=c('group', 'variable')]
                setnames(dat_ctrl, 'V1', 'maxv')

                #Calculate median: Determines ordering of plots by magnitude
                dt1 <- dat[, median(value, na.rm=T), .(group, variable)]
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

                #Collect group parameters
                dt1 <- (data.table(group=grp_ord,
                                         col=grp_colours, vartype=grp_vartype,
                                         grp_geom, ylim1, ylim2, xbrkint, xbrkint_auto))

                #Update with auto-generated parameters where not supplied
                dt1[is.na(xbrkint), xbrkint:=xbrkint_auto]

                #Clean up
                dt1[, c('xbrkint_auto'):=NULL]

                #Add group pars to control table
                dat_ctrl <- merge(dat_ctrl, dt1, by='group')

                #Define group levels: determines ordering of groups
                dat_ctrl[, group:=factor(dat_ctrl$group, levels=grp_ord, ordered=T)]

                #Order variables by group, then by median/alphabetically depending on varord
                ordby <- ifelse(varord[1]=='alph', 'variable', 'med')
                dir   <- ifelse(ordby=='variable', 1, -1)
                setorderv(x=dat_ctrl, cols=c('group', ordby), order=c(1, dir))

                #Calculate relative sizes of variable plots (cm's) based on relative maxima

                  #Plots of variables in const_grps will have width of:
                  constw <- maxw*0.8

                  #Whenever maxv is below xbrkint, extend it to xbrkint
                  dat_ctrl[maxv<xbrkint, maxv:=xbrkint]

                  #Width of each variable's plot is maxw factored by its group's maximum
                  dat_ctrl[, pwidth:=(abs(maxv)/max(abs(maxv)))*maxw, .(group)]

                  #Set plot width of unscaled graphs to half the maximum plot size
                  dat_ctrl[group%in%const_grps, pwidth:=constw]

                  #Extend or shrink ylim2 of each plot by factor of (pwidth/maxw)
                  #Presently, all ylims are the same but plot widths are not, therefore,-
                  #-value axes are not similar in absolute terms
                  #This fixes that, making axes comparable in absolute terms
                  dat_ctrl[!group%in%const_grps, ylim2:=ylim2*(pwidth/maxw)]

              #Add axis tick length to pwidth to account for it in each plot
              dat_ctrl[, pwidth:=pwidth+len_tick]

              #Make group labeller strip
              tstrip <- grp_labStrip(dt=unique(dat_ctrl[,
                                                        list(group, vartype, col)]),
                                     tsize=grp_tsize, grplab=T)

             #Add spacer row below tstrip to ensure nice separation btw tstrip & panelled plots to be put under
             #tstrip <- gtable_add_rows(x=tstrip, height=tstrip$grobs[[1]]$heights, pos=1)

             #Height of group labeller strip
             tstrip_h <- tstrip$grobs[[1]]$heights

              #Calculate height needed for facet titles using some clever trig.
                #Calculate strwidth based on user suppl. text size & current device's par()$ps
                #-Note: calculation assumes strwidth() uses default fontsize of 10, not R's
                #-default of 12. Ensures text rotation, left margin in text are compensated
                #-and,there is a small, nice margin between text labels and bordering elements
                labs_dt <- dat_ctrl[, strwidth(s=as.character(variable),
                                                units="in", cex=var_tsize/10)*2.54,
                                    .(variable, pwidth)]

                setnames(labs_dt, 'V1', 'strhyp')

                #Given that the widths lie along the hypotenuse of a RT angled triangle
                max_hyp <- labs_dt[strhyp==max(strhyp), strhyp][1]
#                 strip_height <- sum((cos(45) * max_hyp),
#                                      as.numeric(tstrip_h), spacer_h, as.numeric(pmargin[[1]]))
                strip_height <- cos(45) * max_hyp
                #strip_height <- max_hyp
                print(max_hyp)

              #Determine RT margin extension of last plot in output in order to accomodate all labels
               #Base of triangle of which the titles form hypotenuses
               labs_dt[, strbase:=cos(45)*strhyp, .(variable)]

               #Cumsum of strbase indicates where we run out of pwidth
               labs_dt[, starts:=(cumsum(pwidth))]
               labs_dt[, reach:=(starts + strbase)]
               mw <- max(labs_dt[, reach]) - sum(labs_dt[, pwidth])

            #Split input data.table into variable-wise list
            vlist <- lapply(X=split(1:nrow(dat),
                                    f=as.character(dat[, variable])),
                            FUN=function(x)dat[x])

            #Graph data (variable-wise)
            gglist  <- stratPlot_varlist(var_list=vlist)
            mw      <- gglist[['mw']]
            gglist  <- gglist[['grob_grp']]

            #Add universal y axis
             #Determine strip text height of y axis plot, ==sum of heights of group & strip.heights
              st_ht <- unit(sum(as.numeric(gglist$grobs[[2]]$grobs[[1]]$heights), strip_height), 'cm')

              #Make y axis grob
              gg_yax <- uniYaxis(tym_brks, st_ht, tstrip_h)

             #Get total width of y axis grob plus that of accompanying title
              axw <- sum(grobWidth(gg_yax$grobs[[1]][['grobs']][gg_yax$grobs[[1]]$layout$name=='ylab'][[1]]),
                         grobWidth(gg_yax$grobs[[1]][['grobs']][gg_yax$grobs[[1]]$layout$name=='ylab'][[1]])*0.1,
                         grobWidth(gg_yax$grobs[[1]][['grobs']]$axis_l))
              axw <- convertX(x=axw, unitTo='cm', valueOnly=F)

             #Add y axis grob as left annotation, occupying a column with just enough width: axw
             gg_grob <- arrangeGrob(gglist, left=gg_yax, padding=axw)

            #Add plot labels (if supplied)
             #Plot label
             if(nchar(plab) > 1){
               plab_grob <- textGrob(label=plab,
                                     gp=gpar(fontsize=ptt_tsize, fontface='bold', col="grey30"))
               gg_grob <- arrangeGrob(gg_grob, top=plab_grob, padding=unit(0.5, units='cm'))
             }

             #Add spacer row at the top for some nice margin
             gg_grob <- gtable_add_rows(x=gg_grob, height=unit(spacer_h, 'cm'), pos=0)

             #Add spacer row at the bottom for some nice margin
             gg_grob <- gtable_add_rows(x=gg_grob, height=unit(spacer_h, 'cm'), pos=3)

             #Return grob object and width in inches
             return(list(plot=gg_grob, width=(sum(dat_ctrl[['pwidth']], mw, as.numeric(axw)))))
}
