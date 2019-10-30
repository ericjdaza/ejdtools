### Install and load packages.
# install.packages("dbConnect")
# install.packages("DBI")
# install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat") )
# install.packages("RPostgres")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages(
#   "https://cran.r-project.org/src/contrib/Archive/caret/caret_0.4-5.tar.gz",
#   repos = NULL,
#   type = "source"
# ) # how to install package to match older version of R: http://manishbarnwal.com/blog/2016/10/05/install_a_package_particular_version_in_R/
# install.packages("caret")
# install.packages("rbenchmark")
# install.packages("naniar")
# install.packages("gtools") # https://rdrr.io/cran/gtools/man/asc.html
# install.packages("LaF") # https://cran.r-project.org/web/packages/LaF/index.html
# install.packages("Hmisc") # Volodymyr made me do this
# install.packages("tidyverse")
# install.packages("usethis")
# install.packages("sparklyr")
# install.packages("SparkR")
# install.packages("cutpointr")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("robustHD") # https://www.rdocumentation.org/packages/robustHD/versions/0.5.1/topics/standardize; https://rdrr.io/cran/robustHD/#vignettes
# install.packages("splitstackshape") # https://rdrr.io/cran/splitstackshape/man/stratified.html for stratified random sampling
# install.packages("parallel") # https://privefl.github.io/blog/a-guide-to-parallelism-in-r/
# install.packages("foreach") # https://privefl.github.io/blog/a-guide-to-parallelism-in-r/
# install.packages("doParallel") # https://privefl.github.io/blog/a-guide-to-parallelism-in-r/
# devtools::install_github("privefl/bigstatsr") # https://rdrr.io/cran/bigstatsr/src/R/apply-parallelize.R, https://www.r-pkg.org/pkg/bigstatsr
# library(dbConnect)
# library(DBI)
library(aws.s3)
library(RPostgres)
library(tidyverse)
library(dplyr)
library(magrittr)
library(caret)
library(rbenchmark)
library(naniar) # https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# http://naniar.njtierney.com/articles/getting-started-w-naniar.html
library(gtools)
# library(LaF)
library(Hmisc)
library(tidyverse)
library(sparklyr) # https://spark.rstudio.com/reference/
library(SparkR)
library(usethis)
library(cutpointr)
library(FactoMineR)
library(factoextra)
library(robustHD)
library(splitstackshape)
library(parallel)
library(foreach)
library(doParallel)
library(bigstatsr)
# # no_cores <- parallel::detectCores()
# no_cores <- bigstatsr::nb_cores()
# doParallel::registerDoParallel(parallel::makeCluster(no_cores))
