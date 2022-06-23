# MASDERtools

## Installation

This package is not yet on CRAN; we aim to submit it to CRAN sometime in 2022. This package is currently in a private repository - this means that there are extra steps to install it. The steps are:

1. Create a personal access token by going to this site: [https://github.com/settings/tokens](https://github.com/settings/tokens). You can leave the expiration at 30 days; click the "repo" access checkbox (first one). Copy the access token - it should be of the form `ghp_` followed by a long alphanumeric string.
2. Install the [SATSdata package](https://github.com/SDSattitudes/SATSdata) if you have not already done so. (This package is needed for the vignette; if you skip the vignette building you do not need to install this package.)
3. Run the following lines, substituting your access token from step 1.

```{r}
library(devtools)
devtools::install_github("SDSattitudes/MASDERtools", build_vignettes = TRUE, ref = "main", auth_token = "YOUR_ACCESS_TOKEN_HERE")
```

This method will also install a vignette demonstrating some of the functions in this package; this installation takes a few minutes while it runs the code. If you wish to skip building the vignette, set `build_vignettes = FALSE`. To access the vignette, use `browseVignettes(package = "MASDERtools")` and click the HTML link in the window that appears.

If you do not have the devtools package, install it with `install.packages("devtools")` in the usual way.

## Status

The package is in a functional state for performing some standard analyses on S-SOMAS data. In addition to adding new features and fixing bugs, the documentation needs to be improved. This is a task that would be reasonable for any student to contribute to - if you are interested, contact Doug.

A vignette has been created to show off some of the core features of the package. We may wish to adapt this vignette to a GitHub page.
