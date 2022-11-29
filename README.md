# plotting library for mrwater outputs

R package **mrwaterplots**, version **1.0.1**

[![CRAN status](https://www.r-pkg.org/badges/version/mrwaterplots)](https://cran.r-project.org/package=mrwaterplots)  [![R build status](https://github.com/pik-piam/mrwaterPlots/workflows/check/badge.svg)](https://github.com/pik-piam/mrwaterPlots/actions) [![codecov](https://codecov.io/gh/pik-piam/mrwaterPlots/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrwaterPlots) 

## Purpose and Functionality

More about what it does (maybe more than one line)
    Use four spaces when indenting paragraphs within the Description.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrwaterplots")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact The package maintainer <beier@pik-potsdam.de>.

## Citation

To cite package **mrwaterplots** in publications use:

Beier F, Heinke J, von Jeetze P, Dietrich J (2022). _mrwaterplots: plotting library for mrwater outputs_. R package version 1.0.1, <URL: https://github.com/pik-piam/mrwaterPlots>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrwaterplots: plotting library for mrwater outputs},
  author = {Felicitas Beier and Jens Heinke and Patrick {von Jeetze} and Jan Philipp Dietrich},
  year = {2022},
  note = {R package version 1.0.1},
  url = {https://github.com/pik-piam/mrwaterPlots},
}
```
