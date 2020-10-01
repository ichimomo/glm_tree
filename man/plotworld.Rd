\name{plotworld}
\alias{plotworld}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Function to show created area stratificaion.  
}
\description{
 Function to show created area stratificaion.  
}
\usage{
plotworld(data, area = NULL, xdiff = 0, ydiff = 0, oldarea = FALSE, as.boundary = FALSE, as.line = FALSE, show.number = TRUE, col.var = NULL, add = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    Specify the data used in conducting make.treeglm.
}
\item{area}{
  Area structure (a vector) determined by make.treeglm.  The length of
  this vector shold be same as nrow(data)
}
  \item{xdiff}{
    Adjustment figure for longitude.  If resolution of longitude data
    is 5 degree and the range of 100-105 degree is expreseed as '100',
    the adjustment should be as 2.5.  
}
\item{ydiff}{
      Adjustment figure for latitude. 
%%     ~~Describe \code{ydiff} here~~
}
  \item{oldarea}{
}
\item{as.boundary}{
  if 'TRUE', show areas by rectangles.  Not
  recommended option.
}
\item{as.line}{
  if 'TRUE',show areas by lines.  Not
  recommended option.  
}

\item{show.number}{
  if 'TRUE', serial number of each area is shown.  
}
\item{col.var}{
  Specify color.  
}
\item{add}{
 if 'TRUE', add to current plot (and disregard the
 following arguments). 
%%     ~~Describe \code{add} here~~
}
\item{\dots}{
  graphical parameters for 'image' may also be passed as
          arguments to this function.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
M. Ichinokawa (ichimomo@fra.affrc.go.jp)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

