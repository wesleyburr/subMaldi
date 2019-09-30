\name{readcsvDir}
\alias{readcsvDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import .csv Spectra from a Directory
}
\description{
Imports all \code{.csv} files in a directory, turns them into a binary data frame containing a "mass" and "Intensity" column, and outputs them as \code{.rda} files into an output directory. Allows for rapid import of many spectral datasets at once.
}
\usage{
readcsvDir(direct, massCol, intenseCol, output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{direct}{the path to the directory where the \code{.csv} files are held.}
  \item{massCol}{a character string; the name of the mass column in the \code{.csv} files.}
  \item{intenseCol}{a character string; the name of the intensity column in the \code{.csv} files.}
  \item{output}{the path to the directory where the exported \code{.rda} files containing the binary spectral data frames should go.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a directory of \code{.rda} files containing binary spectral data frames into the \code{output} file path.
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

## Read all .csv files from August 1st and export them as .rda's to another folder
readcsvDir(direct = "./Data/August_1/Working_Directory/", massCol = "m.z", intenseCol = "I", output = "./R_Studio/Data_Frames/")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
