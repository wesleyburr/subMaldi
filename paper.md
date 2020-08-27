---
title: "subMALDI: an open framework R package for processing irregularly-spaced mass spectrometry data"
tags:
  - mass spectrometry
  - spectral processing
  - MALDI-MS
authors: 
  - name: Kristen Yeh
    orcid: 0000-0002-3411-6816
    affiliation: 1
  - name: Naomi L. Stock
    orcid: 0000-0002-3472-9284
    affiliation: 2
  - name: Theresa Stotesbury
    orcid: 0000-0001-6452-4389
    affiliation: 3
  - name: Wesley Burr
    orcid: 0000-0002-2058-1899
    affiliation: 4
affiliations:
  - name: Forensic Science Program, Trent University
    index: 1
  - name: Water Quality Center, Trent University
    index: 2
  - name: Faculty of Science, Forensic Science, Ontario Tech University
    index: 3
  - name: Faculty of Science, Mathematics, Trent University
    index: 4
date: August 4, 2020
bibliography: paper.bib
output: pdf_document
---


# Summary

Mass spectrometry (MS) is an essential analytical technique used in many fields of science, including chemistry, biology, medicine, and more [@Gross:2011]. Mass spectrometry (MS) has been used in biotechnology studies to sequence biomolecules [@Maux:2001], genotype human DNA for individual identification [@Null:2001], explore single cells [@Jones:2003], and examine objects from outer space [@Fenselau:2003]. The diversity and breadth of applications using MS often yields a wealth of complex data, which requires significant processing before meaningful results can be interpreted. Fortunately, an extensive catalogue of software for analysis of MS data is available to researchers, at varying levels of accessibility [@Gibb:2016].

Modern instrumentation often include proprietary software for spectral processing and analysis (e.g. Bruker Daltonics’ Data Analysis). These tools, though convenient, often fail to provide sufficient documentation of the algorithms employed in the software and have limited analytical capabilities. Other commercial tools are available to supplement these programs (e.g. Agilent Technologies’ MassHunter Profinder and Thermo Scientific’s SIEVE^TM^), however, they come at a cost.  Open source software for analysis of MS data is also available online. These applications are often implemented in a variety of statistical computing languages, including Python (e.g. pyOpenMS) [@Rost:2014], Matlab (e.g. LIMPIC) [@Mantini:2007], C++ (e.g. ProteoWizard) [@Chambers:2012] and R (e.g. MSnbase, MALDIquant) [@Gatto:2012; @Gibb:2012]. While more accessible and well-documented than proprietary software, these available open source applications utilize compressive data structures (e.g. S3 and S4 class objects in R), which can make it difficult for researchers to access their raw spectral data. In order to simplify the organization and processing of mass spectrometry data, we propose the R package `subMALDI`.

`subMALDI` is an open framework tool that permits organization, pre-processing (smoothing, baseline correction, peak detection), and normalization of spectral data sets without masking into S3 or S4 class objects. After every step of processing, the *m/z* and intensity data of each spectrum is readily accessible, providing researchers with a more thorough understanding of the data manipulation that occurs during analysis. As a result of the package's open framework, subMALDI data sets are compatible with functions from a wide variety of other R packages, and user-defined functions are easier to implement and test.

# Statement of Need

`subMALDI` permits the direct comparison of irregularly spaced spectral replicates in an open framework, an important feature that other open source tools do not contain. While matrix-assisted laser desoprtion/ionization (MALDI) mass spectra are often visualized on a continuous scale, the data observed are positive intensity values, corresponding to discretely measured mass-to-charge (*m/z*) values [@Stanford:2016]. When spectral replicates are acquired of a sample, there is variation in the number and value of *m/z* responses with accompanying peaks, resulting in irregularly spaced data. This has implications for the statistical interpretations of inter-and intra-sample comparisons. In order to generate meaningful results from unevenly spaced data, it is essential that the data set be standardized by some means. In the statistical computing language R, replicates must be aligned against the same data structure. 

`subMALDI` processes each raw spectrum with one of several smoothing filters, baseline correction methods, and peak detection algorithms included in the package. The processed spectral intensity values are then aligned to an array of all the theoretically possible *m/z* values in the observed mass range, at a specified resolution. The resulting data frame contains all *m/z* data in the first column, with the intensity data of each spectral replicate in adjacent columns.

`subMALDI` was designed for use by researchers who wish to organize, process, and analyze MS data, while still being able to access their raw data throughout the process. It has been utilized in a scientific article accepted into the *Journal of Forensic Chemistry* (FORC-D-20-00095) and in our laboratory for analysis of MALDI-MS and electrospray-ionization (ESI) MS data. The open framework format of `subMALDI` creates a more transparent pipeline for processing of MS data, where users can easily access their raw data and better understand the processing algorithms that are being executed on their data sets. The `subMALDI` framework is intended to reduce the "black-box" characteristics of MS data analysis and assist students and researchers in obtaining a more thorough understanding of MS and the complex, diverse data sets that it is used to produce.

# Acknowledgement

We are grateful to the Canadian Foundation for Innovation, and the Ontario Research Fund for funding the Bruker SolariX XR MALDI FT-ICR-MS in the Water Quality Centre at Trent University.

# Funding

This work was supported by a Natural Sciences and Engineering Research Council (NSERC) Discovery Grant to W. Burr (2017-04741) and a Vice President Research Fund to T. Stotesbury (2019). Author K. Yeh was supported by two NSERC Undergraduate Student Research Awards (USRA), 2019 and 2020.

# References
