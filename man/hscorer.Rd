\name{hscorer}
\alias{hscorer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to compute pollen scores based on biome, PFT or other schemes.
}
\description{
 Computes the relative contributions of higher groups associated with
 individual pollen taxa in a pollen assemblage. Pollen taxa are first mapped to
 their respective higher groups e.g., PFT's or biomes. Scores and percentages
 (of biomes or PFTs depending on the scheme used) are then computed as
 per \href{'http://link.springer.com/article/10.1007/BF00211617'}{Prentice, Colin, et al, 1996.}

}
\usage{
hscorer(pcounts, hscheme)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pcounts}{
  A data.table of pollen counts. The table should have the following columns:
  entitynum: core ID's: identifies pollen cores uniquely
  samplenum: sample ID's: identifies samples within pollen cores uniquely
  varnum: code that represents an observed taxon
  count: abundance of a given varnum: can be raw count or percentage.

}
  \item{hscheme}{
  A data.table of the scoring scheme to use. This can be a biome or PFT scheme.
  For easy identification, the scheme's name should indicate the scheme type e.g.
  'Tarasov_biome_scheme'.

  The first column in hscheme should be named 'varnum'and has the same meaning as
  'varnum'. The 'varnum' column in 'p_counts' maps to that in 'hscheme' to determine
  the higher groups with which to associate individual pollen taxa.

  The rest of the columns should be named with the higher group labels
  i.e., biome1, biome2 etc and filled with 1 or 0 to indicate whether the "varnum"
  in a given row is associated with a higher group or not.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list of two items:

  \item{percentages}{
  a data.table where the first two columns are the entitynum and samplenum
  respectively. The rest of the columns contain the percentages of each higher group
  i.e., relative contributions of each biome, pft etc.
  }

  \item{scores}{
  a data.table where the first two columns are the entitynum and samplenum
  respectively. The rest of the columns contain the scores of each higher group.
  }
}
\references{
Prentice, C., Guiot, J., Huntley, B., Jolly, D., & Cheddadi, R. (1996). Reconstructing biomes from palaeoecological data: a general method and its application to European pollen data at 0 and 6 ka. Climate Dynamics, 12(3), 185-194.}
\author{
John Shekeine, \href{'http://arve.unil.ch/'}{ARVE Research Group}, University of Lausanne
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~pollen }
\keyword{ ~score }
\keyword{ ~pft }
\keyword{ ~biome }
