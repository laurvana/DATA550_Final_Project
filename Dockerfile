FROM rocker/r-ubuntu as base

RUN apt-get update
RUN apt-get install -y libudunits2-dev
RUN apt-get install -y libproj-dev
RUN apt-get install -y libgdal-dev
RUN apt-get install -y libgeos-dev
RUN apt-get install -y pandoc

RUN mkdir /project
WORKDIR /project

RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE renv/.cache

RUN R -e "renv::restore()"
RUN Rscript -e "install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/testing'), dep=TRUE); renv::snapshot(prompt=FALSE)"

###### DO NOT EDIT STAGE 1 BUILD LINES ABOVE ######

FROM rocker/r-ubuntu

WORKDIR /project
COPY --from=base /project .

COPY Makefile .
COPY Final_Project_Report.Rmd .

RUN mkdir code
RUN mkdir output
RUN mkdir data
RUN mkdir report

COPY code code
COPY data/shape_zcta_restrict.RData data/shape_zcta_restrict.RData
COPY data/s19_adj data/s19_adj

CMD make && mv Final_Project_Report.pdf report