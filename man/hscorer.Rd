% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hscorer.r
\name{hscorer}
\alias{hscorer}
\title{hscorer}
\usage{
hscorer(pcounts, hscheme)
}
\arguments{
\item{pcounts}{A data.table of pollen counts. This table is expected to have the following to have:

- a column named 'entitynum' that uniquely identifies pollen cores i.e. entities in the dataset

- a column named 'samplenum' that uniquely identifies age or depth samples in each pollen core.

- a column named 'varnum' that uniquely identifies observed pollen types i.e., taxa}

\item{hscheme}{A data.table of the scoring scheme to use. This can be a biome or PFT scheme. For easy identification, the scheme's name should indicate the scheme type e.g. 'Tarasov_biome_scheme'.

"hscorer" expects that:


(1) hscheme has a column named 'varnum' that uniquely identifies each plant taxon. The 'varnum' column in 'p_counts' maps to that in 'hscheme' to determine what higher groups in "hscheme" to assocate with the "varnum"s (i.e., taxa) listed in "pcounts".

(2) The rest of the columns in "hscheme" have names corresponding to the higher groups in the scheme e.g., biome1, biome2... or pft1, pft2 etc. These should be filled with 1 or 0 to indicate whether the "varnum" in a given row is associated with a higher group or not.}
}
\value{
A list of two items containing:

(1) $percentages:  a data.table where the first two columns are the entitynum and samplenum respectively. The rest of the columns contain the percentages of each higher group i.e., relative contributions of each biome, pft etc.

(2) $scores: a data.table where the first two columns are the entitynum and samplenum respectively. The rest of the columns contain the scores of each higher group.
}
\description{
Function to compute pollen scores based on biome, PFT or other schemes.
}
\details{
Computes the relative contributions of higher groups associated with individual pollen taxa in a pollen assemblage. Pollen taxa are first mapped to their respective higher groups e.g., PFT's or biomes. Scores and percentages (of biomes or PFTs depending on the scheme used) are then computed as per \href{http://link.springer.com/article/10.1007/BF00211617}{Prentice et. al, 1996.}
}
\section{Examples}{

See \href{http://rpubs.com/shekeine/stratR}{stratR vignette}
}
\author{
John Shekeine, Basil Davis \href{http://arve.unil.ch/}{ARVE Research Group}, University of Lausanne
}
\references{
\href{http://link.springer.com/article/10.1007/BF00211617}{Prentice et. al, (1996). Reconstructing biomes from palaeoecological data: a general method and its application to European pollen data at 0 and 6 ka. Climate Dynamics, 12(3), 185-194.}
}

