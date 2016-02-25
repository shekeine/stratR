#stratPlot makes stratigraphic plots
#It takes dat, which is a list with pollen data for 1 higher group e.g. PFT or biome
#Returns facetted stratigraphic plot of taxa in that group
#dat_grp=dat$Herb

stratPlot_varlist <- function(dat_grpL){
  
                              #Apply stratplot_i over variables in i'th group
                              source("fun/stratPlot_var.R", local=T)
                              gglist <- lapply(X=dat_grpL, FUN=stratPlot_var)
                              
                              #Order gglist plots by group and maxv
                              gglist <- gglist[as.character(dat_ctrl[, variable])]
                              
                              #Stitch result into one plot with all variables (taxa) in i'th group

                                #Make stitch args
                                args.list <- list(ncol=length(gglist), widths=lapply(c(dat_ctrl[, pwidth]), unit, "cm"))
                                
                                #Stitch
                                grob_grp   <- do.call(arrangeGrob, c(gglist, args.list))

                                #Make every title space(strip.text) have the height of the tallest title space (i.e. longest var label)
                                  for(nm in names(gglist)){
                                    grob_grp[["grobs"]][[nm]]$heights[grob_grp[["grobs"]][[nm]]$layout$name=="strip_t-1"] <- unit(strip_height, 'cm')
                                    grob_grp[["grobs"]][[nm]]$grobs[["strip_t"]]$heights <- unit(1, 'native')
                                  }
                                  
                                  #Turn title clipping off to allow full display of facet titles even when rotated
                                  for(nm in names(gglist)){grob_grp[["grobs"]][[nm]]$grobs$strip_t$layout$clip <- "off"}
                                  
                                  #Reverse z index to allow left-right overlap
                                  grob_grp$layout$z <- rev(grob_grp$layout$z)

                              #Make labeller plot
                              tstrip <- grp_labStrip(dt_col)

                              #Append group labels as a top strip
                                #Get height of strip
                                glab_ht <- unit((as.numeric(tstrip$grobs[[1]]$heights)), 'cm')
                                grob_grp <- arrangeGrob(grob_grp, top=tstrip, clip=F, padding=glab_ht)
                                
#                               grid.newpage()
#                               grid.draw(grob_grp)

                              #Add right margin to ensure that last label fits in the plot
                              #margin width assumes that each char of the final plot's label takes up 0.2cm
                              mw <- nchar(as.character(dat_ctrl[nrow(dat_ctrl), variable])) * 0.25
                              grob_grp <- gtable_add_cols(grob_grp, unit(mw,"cm"))
                                    
#                               grid.newpage()
#                               grid.draw(grob_grp)
                              
                            return(grob_grp)
                          }