FROM rocker/tidyverse:latest

RUN echo CRAN\tCRAN\t\"https://cloud.r-project.org/src/contrib\"\tTRUE\tTRUE\tTRUE\tTRUE >> /usr/local/lib/R/etc/repositories

RUN R -e "install.packages(c('plyr', 'gridExtra', 'vdiffr', 'diffviewer'))"

RUN R -e "install.packages('tinytex')"  -e "tinytex::install_tinytex()"

ENV PATH="$PATH:/root/bin"

RUN apt update && apt install -y tidy

