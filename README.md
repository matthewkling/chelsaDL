# chelsaDL

The [CHELSA climate dataset](http://chelsa-climate.org/downloads/) is pretty great. This R library has some simple utilities for downloading batches of CHELSA data from within R, and for parsing the names of CHELSA files to identify key metadata variables like GCM names and emissions scenarios. [NOTE: this package is no longer actively maintained. It accesses CHELSA v1, which has now been superseded by CHELSA v2.]

```r
ch_queries(variables = c("temp", "prec"), models = "ACCESS1-0", 
           scenarios = c("rcp45", "rcp85"), timeframes = "2061-2080", 
           layers = 1:12) %>%
    ch_dl()
```

