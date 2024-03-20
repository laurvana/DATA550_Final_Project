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
	rm -f output/*.rds && rm -f output/*.png && rm -f .finalfigs && rm -f Final_Project_Report.pdf 