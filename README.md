# MASDERtools

## Description

The MASDER project is developing four linked survey instruments, and similar analyses are performed after each round of data collection. The MASDERtools package consists of functions written to facilitate the analysis of these survey instruments, though they may be useful for any survey whose items are organized into scales. This package provides functions that will run analyses applied to each scale in the survey with a single command (e.g., creating categorical PCA plots from the Gifi package for each scale with a single call of the `princaller` function). There are also functions to expedite commonly performed tasks (such as creating a Sankey diagram from the results of an EFA or creating a string to be used with the lavaan package). To use these functions, the items are assumed to be stored in a data.frame with names of the form `Construct_N` where N is an integer giving the item number within a scale (as opposed to the item number on the overall survey instrument). See the vignette for more details. 

## Installation

This package is not yet on CRAN (though submission to CRAN is an eventual goal). For now, install the latest version of the package by running this code:

```{r}
library(devtools)
devtools::install_github("SDSattitudes/MASDERtools")
```

If you do not have the devtools package, install it with `install.packages("devtools")` in the usual way.

A vignette demonstrating some of the functions in this package is available but not installed by default because it takes a few minutes to run the code. If you wish to build the vignette, set `build_vignettes = TRUE` as shown below. To access the vignette, use `browseVignettes(package = "MASDERtools")` and click the HTML link in the window that appears.

```{r}
library(devtools)
devtools::install_github("SDSattitudes/MASDERtools", build_vignettes = TRUE)
```

You may need to install the [SATSdata package](https://github.com/SDSattitudes/SATSdata) to build the vignette.

## Status

### Documentation

The package is in a functional state for performing some standard analyses on pilot data collected using the S-SOMAS, S-SOMADS, and I-SOMAS instruments. In addition to adding new features and fixing bugs, the documentation needs to be improved: currently parameters are documented, but return values and examples are not provided. A vignette demonstrating the This is a task that would be reasonable for any student to contribute to - if you are interested, contact Doug.

### Release

As indicated by the [version number](https://semver.org/), MASDERtools is in an *alpha* release state. While it is used by the MASDER team for data analysis, the function interfaces are not finalized and might change at any time before version 1.0.0. For example, `princaller` takes the argument `scale.names`, but this will likely be changed to `scale_names` in a future release. (However, the package is becoming more stable and changes to the interface will be documented if they will break backward compatibility.)

## Funding Acknowledgement

This material is based upon work supported by the National Science Foundation under Grant No. DUE-2013392.
