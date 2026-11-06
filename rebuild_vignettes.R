# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd(paste0(base_dir, "/vignettes/"))

knitr::knit("netinf-quickstart.Rmd.original", output = "netinf-quickstart.Rmd")
knitr::knit("netinf-detailedguide.Rmd.original", output = "netinf-detailedguide.Rmd")

setwd(base_dir)
