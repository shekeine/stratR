#Call helper functions
stratPlot <- function(dat=dat,
                      tymcol='agebp',
                      varcol='variable',
                      valcol='value',
                      varord=c('alph', 'med')[1],

                      grp_ord=unique(dat[, group]),
                      grpcol='group',
                      grpgeom=c('area', 'line')[1],
                      grp_vartype='',
                      const_grps=NULL,
                      gcols=brewer.pal(n=length(unique(dat[, group])), name='Dark2'),

                      xlim1=NULL,
                      xbline=NULL,
                      xbrkint=5,
                      ybrkint=500,

                      axs_tsize=20,
                      var_tsize=22,
                      grp_tsize=25,
                      ptt_tsize=25,

                      plab='',
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

                #xlim1: left extent of plots in each group. This is actually ylim1 with coord_flip()
                if(!length(xlim1)){
                  dt1 <- dat[, min(value, na.rm=T), .(group)][group%in%grp_ord, ]
                  dt1[, xbrkint:=xbrkint] ; setnames(dt1, 'V1', 'gmin')
                  ylim1 <- dt1[, round_any(gmin, xbrkint, floor)]
                }else{ylim1 <- xlim1}

                #Collect group-level parameters
                dt_col <- data.table(group=grp_ord, col=gcols,
                                     vartype=grp_vartype, geom=grpgeom, ylim1=ylim1)

                #Get height needed for all facet titles
                dt_strip <- data.table(vnames=unique(dat[, variable]))
                gt_strip <- arrangeGrob(ggplot(data=dt_strip) + facet_wrap(~vnames, ncol=nrow(dt_strip)) + geom_blank() +
                                          theme(strip.text=element_text(size=20, angle=45)))
                strip_height   <- as.numeric(gt_strip[['grobs']][[1]][['heights']][3])

              #Variable-wise parameters
                #Make Ctrl table with each variable's maximum rate of occurence: used 2set plot extent and labels
                dat_ctrl <- dat[, max(value, na.rm=T), by=c('group', 'variable')]
                setnames(dat_ctrl, 'V1', 'maxv')

                #Add median
                dt1 <- dat[, median(value, na.rm=T), by=c('group', 'variable')]
                setnames(dt1, 'V1', 'med')
                dat_ctrl <- merge(dat_ctrl, dt1, by=c('group', 'variable'))

                #xbline: baseline position along x. This is actually a y coordinate with coord_flip()
                #If undefined, compute it from each variable's median
                #If defined (should be done at group level), merge to match with respective variables
                if(!length(xbline)){
                  dt1 <- dat[, median(value, na.rm=T), .(variable)]
                  setnames(dt1, 'V1', 'xbline')
                  dat_ctrl <- merge(dat_ctrl, dt1, by='variable')
                }else{
                  dat_ctrl <- merge(dat_ctrl, data.table(group=grp_ord, xbline), by='group')
                }

                #Define group levels: determines ordering of groups
                dat_ctrl[, group:=factor(dat_ctrl$group, levels=grp_ord, ordered=T)]

                #Order variables by group, then by med or alphabetically depending on varord
                ordby <- ifelse(varord[1]=='alph', 'variable', 'med')
                dir   <- ifelse(ordby=='variable', 1, -1)
                setorderv(x=dat_ctrl, cols=c('group', ordby), order=c(1, dir))

                #Make DT withvalue break specs
                vbrk_dt <- data.table(group=unique(dat_ctrl[, group]), ylim1, xbrkint)

                #Determine relative sizes of variable facets (in cm) based on relative abundances (median)

                  #Set plot width (cm's) of variable with highest maxv
                  #Set constant plot width for groups whose plots are not to be scaled
                  maxw  <- 5
                  constw <- 5/3

                  #ylim[2] of each variable will be maxv rounded up to the nearest 5
                  dat_ctrl[, ylim2:=roundUp(x=maxv,to=5)]

                  #Width of each variable's facet is maxw factored by that variable's med/global med
                  dat_ctrl[, pwidth:=(med/max(dat_ctrl[, med]))*maxw]

                  #set pwidths that are too low to some constant
                  dat_ctrl[pwidth < 0.5, pwidth:=0.5]

                  #Set plot width of unscaled graphs to half the maximum plot size
                  dat_ctrl[group%in%const_grps, pwidth:=constw]

                  #All plots/variables whose plots have constant width should have the same ylim2
                  #this will be the group-wise maximum to accomodate all vars
                  dat_ctrl[group%in%const_grps, ylim2:=max(ylim2), .(group)]

                #Split input data into variable-wise list
                dat_var <- lapply(X=split(1:nrow(dat), f=as.character(dat[, variable])), FUN=function(x)dat[x])

                #Graph each variable (group-wise)
                gglist <- stratPlot_varlist(dat_var)
                glab_ht <- gglist$glab_ht
                gglist  <- stratPlot_varlist(dat_var)$grob_grp

            #Add universal y axis
             #Determine strip text height of y axis plot==sum of heights of group & strip.heights
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
