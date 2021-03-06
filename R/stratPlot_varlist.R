##'stratPlot_varlist
#'
##'makes stratigraphic plots for the n variables in the input data
##'
##'@param var_list, a list with the original data split into n list items (variable-wise)
##'@return gridded stratigraphic plot of with all the variables and height of top strip that has group labels

stratPlot_varlist <- function(var_list){

                              #Apply stratplot_i over variables in i'th group
                              gglist <- lapply(X=var_list, FUN=stratPlot_var)

                              #Order gglist plots by group and median
                              gglist <- gglist[as.character(dat_ctrl[, variable])]

                              #Stitch result into one plot with all variables (taxa) in i'th group
                                #Make stitch args
                                args.list <- list(ncol=length(gglist), widths=lapply(c(dat_ctrl[, pwidth]), unit, "cm"))

                                #Stitch, message "Each group consists of only 1 observation..." supressed
                                #by suppressMessages(), since group=1 in the constituent grobs doesn't supress it
                                grob_grp  <- suppressMessages(do.call(arrangeGrob, c(gglist, args.list)))

                              #Make every title space(strip.text) have the height of the tallest title space (i.e. longest var label)
                              for(nm in names(gglist)){
                                  grob_grp[["grobs"]][[nm]]$heights[grob_grp[["grobs"]][[
                                    nm]]$layout$name=="strip_t-1"] <- unit(strip_height, 'cm')
                                  grob_grp[["grobs"]][[nm]]$grobs[["strip_t"]]$heights <- unit(1, 'native')
                              }

                              #Turn title clipping off to allow full display of facet titles even when rotated
                              for(nm in names(gglist)){grob_grp[["grobs"]][[nm]]$grobs$strip_t$layout$clip <- "off"}

                              #Reverse z index to allow left-right overlap
                              grob_grp$layout$z <- rev(grob_grp$layout$z)

                              #Append group labels as a top strip
                              grob_grp <- arrangeGrob(grob_grp, top=tstrip, clip=T, padding=tstrip_h)

                                ##Make variable type labeller (to be appended at the bottom)
                                if(length(grp_vartype) >= 1){
                                 bstrip <- grp_labStrip(dt=unique(dat_ctrl[,
                                                                           list(group, vartype, col)]),
                                                        tsize=axs_tsize, grplab=F)
                                 vtyp_ht <- unit((as.numeric(bstrip$grobs[[1]]$heights)), 'cm')
                                 grob_grp <- arrangeGrob(grob_grp, bstrip, ncol=1,
                                                         heights=unit.c(unit(1, 'null'), vtyp_ht), padding = unit(0, "cm"))
                                }

                              #Add right margin extension to ensure that last label fits in the plot
                              grob_grp <- gtable_add_cols(grob_grp, unit(mw,"cm"))

                            return(list(grob_grp=grob_grp, tstrip_h=tstrip_h, mw=mw))
                          }
