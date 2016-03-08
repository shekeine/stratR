###################Compute hgroup scores and percentages based on supplied scheme###############

hscorer <- function(pcounts, hscheme) {

  #Pre-process hgroup scheme

    #Coerce input hscheme to datatable for efficiency
    setDT(hscheme)

    #Keep only relevant hgroup labels i.e., 1's: Removes taxa not in any hgroup
      #Reshape hgroup scheme to long format
      hscheme <- melt(data=hscheme, id.vars="varnum", variable.name='hgroup', value.name='value')

      #Drop all zero labels
      hscheme <- hscheme[!value==0]
      hscheme[, value:=NULL]

      #Setkey
      setkey(hscheme)

  #Pre-process pcounts

      #Coerce input hscheme to datatable for efficiency
      setDT(pcounts)
      setkey(pcounts)

      #Remove taxa not in hscheme
      pcounts <- pcounts[varnum%in%hscheme[, varnum]]

      #Sum counts by varnum : removes repetitions from multiple varnames being assigned to the same varnum
      setkey(pcounts)
      pcounts <- unique(pcounts[, count:=sum(count), by=list(entitynum, samplenum, varnum)])

  #Compute variable (taxa) percentages (does not matter if input count column is already % or counts)

      #Variable (taxa) %'s
      pcounts[, taxa_pc:=( (count) / sum(count)) * 100, .(entitynum, samplenum)]

      #Remove trace variables (taxa)
      pcounts <- pcounts[taxa_pc>=0.5]

  #Compute taxon scores: Normalized taxa percentages
      pcounts[, tscore:=sqrt(taxa_pc)]

  #Join to hscheme in order to append hgroup label (Note, allow.cartesian=T to account4
    #varnums that belong to multiple hgroups e.g. when using pft and biome schemes
     pcounts <- merge(x=pcounts, y=hscheme, by='varnum', all.x=T, allow.cartesian=T)

  #Compute hgroup scores i.e., .sum aggregates of taxon scores by hgroup
     pcounts[, hscores:=sum(tscore), by=list(entitynum, samplenum, hgroup)]

  #Compute hgroup percentages
    pcounts[, hcount:=sum(count), .(entitynum,samplenum, hgroup)]
    pcounts[, hpercs:=( hcount / sum(count)) * 100, .(entitynum,samplenum)]

  #Extract outputs

    #Set key
    setkey(pcounts)

    #hscores: note unique()
    hscores <- unique(pcounts[, list(entitynum, samplenum, hgroup, hscores)])
    hscores[, hscores:=round(hscores, 2)]
    hscores <- dcast.data.table(data=hscores, entitynum + samplenum ~ hgroup,
                                value.var='hscores', fill=0, drop=F)
    #h_percentages
    hpercs <- unique(pcounts[, list(entitynum, samplenum, hgroup, hpercs)])
    hpercs[, hpercs:=round(hpercs, 2)]
    hpercs <- dcast.data.table(data=hpercs, entitynum + samplenum ~ hgroup,
                               value.var='hpercs', fill=0, drop=F)

  #Recast hscores to wide format
    res <- list(percentages=hpercs, scores=hscores)
    return(res)
}
