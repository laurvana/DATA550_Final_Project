FROM dbenkeser/inla

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