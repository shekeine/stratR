###################Compute hgroup scores and percentages based on supplied scheme###############
#'hscorer
#'
#'Function to compute pollen scores based on biome, PFT or other schemes.
#'

#'@details Computes the relative contributions of higher groups associated with individual pollen taxa in a pollen assemblage. Pollen taxa are first mapped to their respective higher groups e.g., PFT's or biomes. Scores and percentages (of biomes or PFTs depending on the scheme used) are then computed as per \href{http://link.springer.com/article/10.1007/BF00211617}{Prentice et. al, 1996.}
#'
#'@param pcounts A data.table of pollen counts. This table is expected to have the following to have:
#'
#'- a column named 'entitynum' that uniquely identifies pollen cores i.e. entities in the dataset
#'
#'- a column named 'samplenum' that uniquely identifies age or depth samples in each pollen core.
#'
#'- a column named 'varnum' that uniquely identifies observed pollen types i.e., taxa
#'
#- a column named 'count' indicating the counts associated with each pollen type i.e., varnum.

#'@param hscheme A data.table of the scoring scheme to use. This can be a biome or PFT scheme. For easy identification, the scheme's name should indicate the scheme type e.g. 'Tarasov_biome_scheme'.
#'
#'"hscorer" expects that:
#'
#'
#'(1) hscheme has a column named 'varnum' that uniquely identifies each plant taxon. The 'varnum' column in 'p_counts' maps to that in 'hscheme' to determine what higher groups in "hscheme" to assocate with the "varnum"s (i.e., taxa) listed in "pcounts".
#'
#'(2) The rest of the columns in "hscheme" have names corresponding to the higher groups in the scheme e.g., biome1, biome2... or pft1, pft2 etc. These should be filled with 1 or 0 to indicate whether the "varnum" in a given row is associated with a higher group or not.

#'@return A list of two items containing:
#'
#'(1) $percentages:  a data.table where the first two columns are the entitynum and samplenum respectively. The rest of the columns contain the percentages of each higher group i.e., relative contributions of each biome, pft etc.
#'
#'(2) $scores: a data.table where the first two columns are the entitynum and samplenum respectively. The rest of the columns contain the scores of each higher group.

#'@references \href{http://link.springer.com/article/10.1007/BF00211617}{Prentice et. al, (1996). Reconstructing biomes from palaeoecological data: a general method and its application to European pollen data at 0 and 6 ka. Climate Dynamics, 12(3), 185-194.}
#'@author  John Shekeine, Basil Davis \href{http://arve.unil.ch/}{ARVE Research Group}, University of Lausanne
#'
#'@section Examples:
#'See \href{http://rpubs.com/shekeine/stratR}{stratR vignette}

hscorer <- function(pcounts, hscheme) {

  #Pre-process hgroup scheme

    #Coerce input hscheme to DT for efficiency
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
