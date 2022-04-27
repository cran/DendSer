\name{costARc}
\alias{costARc}
\alias{costBAR}
\alias{costED}
\alias{costLS}
\alias{costLPL}
\alias{costPL}
\title{Cost functions for DendSer}
\description{
  Each of these functions evaluates the cost of an ordering.
}
\usage{
costARc(sw, o, target = nrow(sw) - 1, ...)
costBAR(sw, o, target=max(2,floor(nrow(sw)/5)),...)
costLS(sw, o, target=seq_along(sw),...)
costPL(sw, o, ...)
costLPL(sw, o, target=(nrow(sw)-1):1,...)
costED(sw, o, node,se,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sw}{
    For cost=costLS, this is a vector of object weights. Otherwise is a   symmetric matrix.
}
  \item{o}{
     An ordering vector.
}
  \item{node}{
     The node 
}
  \item{se}{
     Extra info
}

 \item{target}{
     Parameter used by cost function.
}
  \item{\dots}{
     Other args.
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{Result of cost}
% \references{
% }
\author{
	Catherine Hurley & Denise Earle
}

