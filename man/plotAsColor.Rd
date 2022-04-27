\name{plotAsColor}
\alias{plotAsColor}
\title{
Function that plots a matrix as a color image}
\description{
	Function that plots a matrix as a color image, in matrix order.
}
\usage{
plotAsColor(m,  order = NULL, order.col = order, order.row = order, 
rank = FALSE, border.color = "grey70", labels = FALSE,x=1:ncol(d),y=1:nrow(d), ...)
}
\arguments{
  \item{m}{
Numeric matrix.
}
  \item{order}{
Default order used for rows and columns.
}
  \item{order.col}{
Column order.
}
  \item{order.row}{
Row order
}
  \item{rank}{
Logical, whether matrix m should be converted to ranks or not.}
  \item{border.color}{
Color of border. Null for no border.
}
  \item{labels}{
If TRUE, add labels obtained from m.
}
  \item{x}{
X coordinates, passed to \code{image}.}
  \item{y}{
Y coordinates, passed to \code{image}.}
  \item{\dots}{
passed to \code{image}.}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{image}}.
}

\examples{
	 			
require(DendSer) 			

d<- dist(scale(iris[,-5]))
plotAsColor(d,dser(d,hmethod="average"))

}
