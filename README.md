### Description
`COVID19` will be a suite of functions implementing different analyses of the COVID-19 publicly available data.

### Data Source
Johns Hopkins University, CSSE (https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data)

### Requirements
R (>= 3.0.1), ggplot2, tidyr, readr

### How to Install within R
```{r}
require(devtools)
source_url("https://github.com/RayDebashree/COVID19/blob/master/covid19plot_v1.R?raw=TRUE")
```
It is recommended to download/copy the stand-alone R program in this repository, save it in your local directory of choice and `source()` it from your local directory. When a new version of the software is available, older versions may be removed from this repository, and the above `devtools::source_url()` technique may not work.

### An Example
```{r}
covid19plot(start.date="02-01-2020", end.date=NULL, plot.variable="Confirmed", countries=c('India','US','Italy'))
```
![An example](https://pbs.twimg.com/media/ETQRHixWsAE8GEP?format=jpg&name=4096x4096)
