\name{readcsvDir}
\alias{readcsvDir}
\title{
Import .csv Spectra from a Directory
}
\description{
Imports all \code{.csv} files in a directory, turns them into a binary data frame containing a "mass" and "Intensity" column, and outputs them as \code{.rda} files into an output directory. Allows for rapid import of many spectral datasets at once.
}
\usage{
readcsvDir(direct, massCol, intenseCol, output)
}
\arguments{
  \item{direct}{The path to the directory where the \code{.csv} files are held.}
  \item{massCol}{A character string; the name of the mass column in the \code{.csv} files.}
  \item{intenseCol}{A character string; the name of the intensity column in the \code{.csv} files.}
  \item{output}{The path to the directory where the exported \code{.rda} files containing the binary spectral data frames should go.}
}

\value{
Returns a directory of \code{.rda} files containing binary spectral data frames into the \code{output} file path.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
Wesley Burr <wburr@trentu.ca>
}



\keyword{ methods }
