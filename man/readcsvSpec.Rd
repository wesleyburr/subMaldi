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
  \item{spec_file}{a character string; the file path to the \code{.csv} spectrum that is to be imported.}
  \item{massCol}{a character string; the name of the mass column in the spectrum's \code{.csv} file.}
  \item{intenseCol}{a character string; the name of the intensity column in the spectrum's \code{.csv} file.}
  \item{spec_name}{a character string; the name of the new, binary data frame containing imported the \emph{m/z} and intensity data.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a binary data frame containing the imported data in a \emph{m/z} column denoted "mass", and an intensity column denoted "Intensity".
}
\references{
%% ~put references to the literature/web site here ~
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
## Read a .csv file in the folder containing August 1st's working directory and import it do a data frame called "Samp_01"
Samp_01 <- readcsvSpec(spec_file = "./Data/August_1/Working_Directory/Sample_01.csv", massCol = "m.z", intenseCol = "I")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
