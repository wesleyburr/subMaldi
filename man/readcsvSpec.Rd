\name{readcsvSpec}
\alias{readcsvSpec}
\title{
Import a Single .csv Spectrum
}
\description{
Imports a single spectrum in .csv format and turns it into a binary data frame containing a "mass" and "Intensity" column.
}
\usage{
readcsvSpec(spec_file, massCol, intenseCol)
}
\arguments{
  \item{spec_file}{A character string; the file path to the \code{.csv} spectrum that is to be imported.}
  \item{massCol}{A character string; the name of the mass column in the spectrum's \code{.csv} file.}
  \item{intenseCol}{A character string; the name of the intensity column in the spectrum's \code{.csv} file.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a binary data frame containing the imported data in a \emph{m/z} column denoted "mass", and an intensity column denoted "Intensity".
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
Wesley Burr <wburr@trentu.ca>
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}


\keyword{ methods }
