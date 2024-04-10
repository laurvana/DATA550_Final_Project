# Code Description

`code/01_create_table.R`

- load clean data from `data/` folder
- save table 1 in `output/` folder

`code/02_make_figures.R`

- load clean data from `data/` folder
- load map from `data/` folder
- save final figure forest plots 1-3 in `output/` folder

**Please Note** If you need to install the package `INLA` on your device, please follow the instructions in `code/02_make_figures.R` to aid in the install process. These lines of code have been commented out to decrease run time in case INLA has already been installed on your device.

`code/03_render_report.R`

- render `code/Final_Project_Report.Rmd` 
- save compiled report in main folder

`Final_Project_Report.Rmd`

- load clean data from `data/` folder
- includes brief description of data
- load table 1 from `output/` folder
- includes brief interpretation of table 1
- load final figure forest plots 1-3 from `output/` folder
- includes brief description of figures
- save pdf version of report in main folder

In order to generate the final report please run the following command in your terminal: `make Final_Project_Report.pdf`. This command should generate the table 1 and three associated figures as well as their interpretations into a final pdf document.

`make install` 

- a make rule used to to sync user's package library with the project's lockfile

`make clean` 

- a make rule used to to remove previously generated figures, tables, and pdf reports
