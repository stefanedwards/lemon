FROM rocker/tidyverse:latest

RUN echo CRAN\tCRAN\t\"https://cloud.r-project.org/src/contrib\"\tTRUE\tTRUE\tTRUE\tTRUE >> /usr/local/lib/R/etc/repositories

RUN R -e "install.packages(c('plyr', 'gridExtra', 'vdiffr', 'diffviewer'))"


