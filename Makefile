# REPORT ASSOCIATED RULES (run within docker container)
Final_Project_Report.pdf: Final_Project_Report.Rmd code/03_render_report.R\
  output/table_one.rds\
  .finalfigs
	Rscript code/03_render_report.R

output/table_one.rds: code/01_create_table.R
	Rscript code/01_create_table.R
	
# creates output/*finalfigs[1-3].png
.finalfigs: code/02_make_figures.R
	Rscript code/02_make_figures.R && touch .finalfigs

.PHONY: clean
clean:
	rm -f output/*.rds && rm -f output/*.png && rm -f .finalfigs && rm -f Final_Project_Report.pdf && rm -f report/*.pdf
	
.PHONY: install
install:
	Rscript -e "renv::restore(prompt=FALSE)"
	
# DOCKER-ASSOCIATED RULES (run on local machine)

PROJECTFILES = Final_Project_Report.Rmd code/01_create_table.R code/02_make_figures.R code/03_render_report.R Makefile
RENVFILES = renv.lock renv/activate.R renv/settings.json

# rule to build an image
project_image_inla: Dockerfile $(PROJECTFILES) $(RENVFILES)
	docker build -t project_image_inla .
	touch $@

# Rule to build the final report automatically in container
report/Final_Project_Report.pdf: 
	docker run -v "$$(pwd)"/report:/project/report laurvana/project_image_inla:latest
