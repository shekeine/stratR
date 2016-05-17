#'stratPlot
#'
#'stratPlot builds side by side stratigraphic plots of multiple variables i.e., subplots,that are relatively sized and have consistent axis parameters, making it easy to compare multiple time-series with different means.
#'

#'@param dat Required: A data.table, data.frame or matrix with the data to be plotted.
#'@param tymcol Required: Column containing the dataset's temporal dimension.
#'@param varcol Required: Column containing the variable labels.
#'@param valcol Required: Column containing the values associated with the variables in the dataset.
#'@param varord Optional: How should variables be ordered?, "alph" for alphabetically, "med" for ordering based on medians. If "med", variable plots are ordered left to right based on median (highest to lowest).
#'@param grpcol Required: Column containing group labels by which variables will be aggregated.
#'@param grp_ord Optional: Vector indicating groups to plot and their ordering. Only variables associated with groups in this argument will be plotted.
#'@param grp_colours Optional: Vector of colours that will be used symbolize each group: should be of (a) length 1 in which case all groups, and hence plots, have the same colour, or (b) length equal to the number of groups in which case supplied values are applied along "grp_ord".

#'@param grp_geom Optional: Plot geometry in which data in each group should be mapped. This should be of (a) length 1 in which case all plots have the same geometry or, (b) length equal to the number of groups in which case supplied geometries will be applied along "grp_ord".
#'@param grp_bline Optional: X-position of plot baselines. This should be of (a) length 1 in which case all plots have the same baseline or, (b) length equal to the number of groups in which case supplied values are applied along "grp_ord"
#'@param grp_vartype Optional: Vector of X axis labels for each group: should be of (a) length 1 in which case all plots have the same X-axis label, or (b) length equal to the number of groups in which case supplied values are applied along "grp_ord".
#'@param grp_const Optional: Vector indicating groups whose plot widths should not be relatively scaled.
#'@param xbrkint Optional: X-axis break interval. This should be of (a) length 1 in which case all plots have the same X-axis break interval or, (b) length equal to the number of groups in which case supplied values are applied along "grp_ord".
#'@param ybrkint Optional: Analogous to the "xbrkint" interval.
#'@param exag_by optional: Factor by which to exaggerrate area plots in order to amplify very small values.
#'@param plab Optional: Main plot title
#'@param ylab Y-axis title i.e., title to associate with tymcol
#'@param ptt_tsize Optional: Text size of the main plot title; if supplied in the "plab" argument.
#'@param axs_tsize Optional: Text size of axis text
#'@param var_tsize Optional: Text size of variable plot titles. Variable plot titles are read from the column supplied to the "varcol" argument.
#'@param grp_tsize Optional: Text size of group labels. Group labels are read from the column supplied to the "varcol" argument.
#'@param pmargin Optional: Unit object indicating margins to use around variable plots: top, right, bottom, and left.
#'@param maxw Optional: Width that the variable with the maximum range in each group of variables should have (in cms). Widths of the other variables are calculated relative to this value and therefore, this argument determines the total width of the final plot as well.
#'@details StratPlot uses ggplot2 to construct gridded time-series plots of multiple variables. Importantly, it 1) allows plot sizes to be relatively scaled; 2) provides fine control of plotting parameters e.g., axis breaks; 3) is designed for the visualisation of stratigraphic datasets, which often have many variables of different kinds but can be used or adapted to work with other time-series data.

#'@return A list of length 2 containing:
#'
#'(1) $plot:  A plot object of class "grob" from which a graphical output can be produced with \code{\link{grid}} or \code{\link{grid.arrange}}
#'
#'(2) $width: Width of the generated plot (in cm)

#'@references
#'
#'@author  John Shekeine, Basil Davis \href{http://arve.unil.ch/}{ARVE Research Group}, University of Lausanne
#'
#'@section Examples:
#'See \href{http://rpubs.com/shekeine/stratR}{stratR vignette}
#'
#'@export
#'
stratPlot <- function(dat=dat,
                      tymcol='agebp',
                      varcol='variable',
                      valcol='value',
                      varord=c('alph', 'med')[1],

                      grpcol='group',
                      grp_ord=unique(dat[, get(grpcol)]),
                      grp_colours=distinctColorPalette(k=length(grp_ord)),
                      grp_geom=c('area', 'line')[1],
                      grp_bline=NA,
                      grp_vartype=NA,
                      grp_const=NA,

                      xbrkint=NA,
                      ybrkint=NA,
                      exag_by=5,

                      plab='',
                      ylab='Time',

                      ptt_tsize=22,
                      axs_tsize=18,
                      var_tsize=20,
                      grp_tsize=20,

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
              dat <- copy(dat)
              setnames(x=dat, old=c(tymcol, varcol, valcol, grpcol),
                       new=c('agebp', 'variable', 'value', 'group'))

              #Subset and order as per grp_ord argument
              dat <- dat[group%in%grp_ord, ]

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

                #Guarantee some margin betw plot geom and axes
                # if((last(tym_brks)-max(dat[, agebp], na.rm=T))){
                #   tym_brks <- c(tym_brks, last(tym_brks) + ybrkint)
                # }
                # if((tym_brks[1]==min(dat[, agebp], na.rm=T))){
                #   tym_brks <- c(tym_brks[1] - ybrkint, tym_brks)
                # }
                if((abs(last(tym_brks) - max(dat[, agebp]))) < (0.1*ybrkint)){
                  tym_brks <- c(tym_brks, last(tym_brks) + ybrkint)
                  }
                if((abs(tym_brks[1] - min(dat[, agebp]))) < (0.1*ybrkint)){
                  tym_brks <- c(tym_brks[1] - ybrkint, tym_brks)
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

                #Collect group parameters
                dt1 <- (data.table(group=grp_ord,
                                         col=grp_colours, vartype=grp_vartype,
                                         grp_geom, grp_bline, ylim1, ylim2, xbrkint, xbrkint_auto))

                #Update with auto-generated parameters where not supplied
                dt1[is.na(xbrkint), xbrkint:=xbrkint_auto]

                #Clean up
                dt1[, c('xbrkint_auto'):=NULL]

                #Add group pars to control table
                dat_ctrl <- merge(dat_ctrl, unique(dt1), by='group')

                #Where grp_bline is "med", set to median
                dat_ctrl[grp_bline=="med", grp_bline:=as.character(med)]
                dat_ctrl[, grp_bline:=as.numeric(grp_bline)]

                #Define group levels: determines ordering of groups
                dat_ctrl[, group:=factor(dat_ctrl$group, levels=grp_ord, ordered=T)]

                #Order variables by group, then by median/alphabetically depending on varord
                ordby <- ifelse(varord[1]=='alph', 'variable', 'med')
                dir   <- ifelse(ordby=='variable', 1, -1)
                setorderv(x=dat_ctrl, cols=c('group', ordby), order=c(1, dir))

                #Calculate relative sizes of variable plots (cm's) based on relative maxima

                  #Plots of variables in grp_const will have width of:
                  constw <- maxw*0.8

                  #Whenever maxv is below xbrkint, extend it to xbrkint
                  dat_ctrl[maxv<xbrkint, maxv:=xbrkint]

                  #Width of each variable's plot is maxw factored by its group's maximum
                  dat_ctrl[, pwidth:=(abs(maxv)/max(abs(maxv)))*maxw, .(group)]

                  #Set plot width of unscaled graphs to half the maximum plot size
                  dat_ctrl[group%in%grp_const, pwidth:=constw]

                  #Extend or shrink ylim2 of each plot by factor of (pwidth/maxw)
                  #Presently, all ylims are the same but plot widths are not, therefore,-
                  #-value axes are not similar in absolute terms
                  #This fixes that, making axes comparable in absolute terms
                  dat_ctrl[!group%in%grp_const, ylim2:=ylim2*(pwidth/maxw)]

              #Add axis tick length to pwidth to account for it in each plot
              dat_ctrl[, pwidth:=pwidth+len_tick]

              #Make group labeller strip
              tstrip <- grp_labStrip(dt=unique(dat_ctrl[,
                                                        list(group, vartype, col)]),
                                     tsize=grp_tsize, grplab=T)

             #Height of group labeller strip
             tstrip_h <- tstrip$grobs[[1]]$heights

              #Calculate height needed for facet titles using some clever trig.
                #Calculate strwidth based on user suppl. text size & current device's par()$ps
                #-Note: calculation assumes strwidth() uses default fontsize of 10, not R's
                #-default of 12. Ensures text rotation, left margin in text are compensated
                #-and,there is a small, nice margin between text labels and bordering elements
                labs_dt <- dat_ctrl[, sum(strwidth(s=as.character(variable),
                                                units="in", cex=(var_tsize*1.5)/(par()$ps))*2.54,
                                          as.numeric(tstrip_h)), .(variable, pwidth)]

                setnames(labs_dt, 'V1', 'strhyp')

                #Given that the widths lie along the hypotenuse of a RT angled triangle
                max_hyp <- labs_dt[strhyp==max(strhyp), strhyp][1]
                strip_height <- cos(45) * max_hyp

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
             gg_grob <- gtable_add_rows(x=gg_grob, heights=unit(spacer_h, 'cm'), pos=0)

             #Add spacer row at the bottom for some nice margin
             gg_grob <- gtable_add_rows(x=gg_grob, heights=unit(spacer_h, 'cm'), pos=3)

             #Return grob object and width in inches
             return(list(plot=gg_grob, width=(sum(dat_ctrl[['pwidth']], mw, as.numeric(axw)))))
}
