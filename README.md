# subMaldi

subMALDI is an open framework package for the R programming environment that permits organization,  pre-processing (smoothing, baseline correction, peak detection), and normalization of spectral data sets. As a result of the package's design, subMALDI data objects are compatible with functions from a wide variety of other R packages, and user-defined functions are easier to implement and test.

<center><img src="subMALDIprocessing.png" width="650"></center>

# Installing in R

To install this package in R, a dependency from the BioConductor project are required. Begin by installing the BioConductor Manager:

    install.packages("BiocManager")
    
and then install the package MassSpecWavelet:

    BiocManager::install("MassSpecWavelet")
    
Then install the CRAN-supported packages required:

    install.packages(c("tidyr", "ggplot2", "ggpmisc", "reshape2", "RColorBrewer", "signal"))
    
Once the packages are all installed, download the source copy of the package from the releases folder, and 
install it using type = "source":

    install.packages("subMALDI-1.0.0.tar.gz", type = "source")
   
Update the file name as necessary.


# Getting Started

Refer to the [vignette on workflow](vignettes/subMALDI_workflow.Rmd) for a brief summary of the package functionality, and the [vignette on processing](vignettes/subMALDIprocessing.Rmd) for a simple example.





# Contributing?

Examine our [contribution guidelines](Contributing.md) for more.
