# subMaldi

subMALDI is an open framework package for the R programming environment that permits organization,  pre-processing (smoothing, baseline correction, peak detection), and normalization of spectral data sets. As a result of the package's design, subMALDI data objects are compatible with functions from a wide variety of other R packages, and user-defined functions are easier to implement and test.

<center><img src="subMALDIprocessing.png" width="650"></center>

# Installing in R

To install this package in R, begin by downloading the source copy of the package from the [releases folder](https://github.com/wesleyburr/subMaldi/releases) and 
install it using type = "source". But, before you do this, a dependency from the BioConductor project is required. 
Begin by installing the BioConductor Manager:

    install.packages("BiocManager")
    
and then install the package MassSpecWavelet:

    BiocManager::install("MassSpecWavelet")
    
Then install the CRAN-supported packages required:

    install.packages(c("tidyr", "ggplot2", "ggpmisc", "reshape2", "RColorBrewer", "signal"))
    
Once the packages are all installed, you can then install the package:

    install.packages("subMALDI-1.0-2.tar.gz", type = "source")
   
Update the file name as necessary to whatever the copy you downloaded was set to for versioning.

Note: if you install from source, and set **dependencies = TRUE**, this should all be moot. This
assumes you run the commands exactly as given, in which case, the BioConductor sequence doesn't always
seem to work. For example,

    install.packages("subMALDI-1.0-2.tar.gz", type = "source", dependencies = TRUE)

# Getting Started

Refer to the [vignette on workflow](https://wesleyburr.github.io/subMaldi/articles/subMALDI_workflow.html)
for a brief summary of the package functionality, and the 
[vignette on processing](https://wesleyburr.github.io/subMaldi/articles/subMALDIprocessing.html) for a simple example.

# Contributing?

Examine our [contribution guidelines](Contributing.md) for more.
