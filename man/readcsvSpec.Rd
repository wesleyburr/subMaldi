\name{readcsvSpec}
\alias{readcsvSpec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import a Single .csv Spectrum
}
\description{
Imports a single spectrum in .csv format and turns it into a binary data frame containing a "mass" and "Intensity" column.
}
\usage{
readcsvSpec(spec_file, massCol, intenseCol, spec_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec_file}{A character string; the file path to the \code{.csv} spectrum that is to be imported.}
  \item{massCol}{A character string; the name of the mass column in the spectrum's \code{.csv} file.}
  \item{intenseCol}{A character string; the name of the intensity column in the spectrum's \code{.csv} file.}
  \item{spec_name}{A character string; the name of the new, binary data frame containing imported the \emph{m/z} and intensity data.}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Read the sample file "Blank1.csv"
Blank1 <- readcsvSpec(spec_file = "/subMALDI/data/Blank.csv", massCol = "m.z", intenseCol = "I")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
