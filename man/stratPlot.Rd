\name{stratPlot}
\alias{stratPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
stratPlot
}
\description{
stratPlot builds side by side stratigraphic plots of multiple variables i.e., subplots,
that are relatively sized and have consistent axis parameters,
making it easy to compare multiple time-series with different means.
}
\usage{
stratPlot(dat=dat,
          grpcol='group',
          tymcol='agebp',
          varcol='variable',
          valcol='value',
          varord=c('alph', 'med')[1],

          grp_ord=unique(dat[, group]),
          grp_geom=c('area', 'line')[1],
          grp_vartype='',
          const_grps=NULL,
          grp_colours=brewer.pal(n=length(unique(dat[, group])), name='Dark2'),

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
          pmargin=unit(c(0.05, 0, 0.1, 0), 'cm')
         )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Dataset to use for plotting. This should be a data.table object.
}
  \item{grpcol}{
Name of column that contains group labels for each variable
}
  \item{tymcol}{
Name of column that contains the temporal dimension.
}
  \item{varcol}{
Name of column that contains the variable labels
}
  \item{valcol}{
Name of column that contains values of each variable
}
  \item{varord}{
How should variables be ordered?, "alph" for alphabetically, "med" for ordering
based on relative abundances (based on medians) of the variables.
}
  \item{grp_ord}{
Order of groups associted with input variables: first to last.
}
  \item{grp_geom}{
Geometry to which data in each group should be mapped to. This should be of (a)
length 1 in which case all plots have the same geometry or, (b) length equal to
the number of groups in which case supplied lengths are applied along "grp_ord"
}
  \item{grp_vartype}{
Vector indicating the type of variable in each group. This should be of (a)
length 1 in which case all plots have the same x-axis label, or (b) length equal to
the number of groups in which case supplied labels are applied along "grp_ord"
}
  \item{const_grps}{
Names of groups whose plot widths should not be relatively scaled.
}
  \item{grp_colours}{
Vector of colours to symbolize each group.
}
  \item{xlim1}{
Left extent of plots in each group. This should be of (a) length 1 in
which case all plots have the same left extent or, (b) length equal to the number
of groups in which case supplied lengths are applied along "grp_ord"
}
  \item{xbline}{
X-position of the plot baselines. This should be of (a) length 1 in
which case all plots have the same baseline or, (b) length equal to the number of groups in which case supplied values are applied along "grp_ord"
}
  \item{xbrkint}{
Intervals at which value breaks (i.e., x-axis breaks) are spaced.This should be
of (a) length 1 in which case all plots have the same interval spacing or, (b)
length equal to the number of groups in which case the supplied intervals are
applied along "grp_ord".
}
  \item{ybrkint}{
Intervals at which temporal breaks (i.e., y-axis breaks) are spaced
}
  \item{axs_tsize}{
Text size of axis annotations
}
  \item{var_tsize}{
Text size of subplot titles: subplot titles are in turn read from "varcol"
}
  \item{grp_tsize}{
Text size of group labels: subplot titles are in turn read from "grpcol"
}
  \item{ptt_tsize}{
Text size of plot title
}
  \item{exag_by}{
Factor by which to exaggerrate area plots. Defaults to 5.
 }
  \item{plab}{
Plot title
}
  \item{xlab}{
X-axis title
}
  \item{ylab}{
Y-axis title i.e., title to associate with tymcol
}
  \item{pmargin}{
Margins to put around plots with the sizes of the top, right, bottom, and left
margins. This should be specified as a "unit" object.

}
}
\details{
StratPlot uses ggplot2 to construct one area plot for each variable in the input
data.It then grids all the per-variable plots into one. The per-variable plots
are relatively sized and have consistent axis parameters that can be tuned easily
with 'xbrkint'.
}
\value{
A plot object of class c("gtable", "grob", "gDesc" ) from which a graphical output
can be produced with \code{\link[grid]{grid.draw}} or \code{\link[grid]{grid.arrange}}
}
\references{
%% ~put references to the literature/web site here ~
}
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
\keyword{ ~stratigraphy }
\keyword{ ~ggplot2 }
