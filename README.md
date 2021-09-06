### vefmap: tools for VEFMAP analysis and report

`vefmap` is an R package containing tools to support VEFMAP analysis and reporting, including annual reporting and extended data analyses.

Sets classes on standard analyses and generates automatic reports based on these.

### Installation

Imports several helpers from the `aae.hydro` R package, available at https://github.com/aae-stats/aae.hydro.

```{r}
# install.packages("remotes")
remotes::install_github("aae-stats/vefmap")
```

### Code of Conduct
  
Please note that the vefmap project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.


### Usage

```{r}
# load package
library(vefmap)

# load some data (database query eventually)
load_data()

# set parameters (fuzzy queries and use sci or common names)
species <- c("murraycod", "goldenperch", "murraydarlingrainbowfish")
years <- 2016:2021

# run analyses
analyses <- list(
  hydrology = analyse_hydro(),
  length_frequency = analyse_length(),
  trends = analyse_trends(),
  recruitment = analyse_recruits(),
  age_frequency = analyse_age()
)

# generate annual report
generate_report(analyses)
```
