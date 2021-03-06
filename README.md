# project-initializeR

## Scaffolding for large R Projects in Bioinformatics

### Description: 

Just a try for some workflow/structure setup in order to cope with the
lack of scaffolding tools in R for big projects dedicated to bioinformatics analysis.


---

### Project Scaffolding:
    
Project:
  * info:	pdfs, powerpoints, docs, etc anything that is not affiliated with the scripts
  
  * data-input: data that will be used by the scripts but is not generated by them
  
  * data-output:data that will be generated by the scripts but is not in a form of proper report
  
  * reports:    only files that can be shown to someone else in a proper form
  
  * src:		all the R scripts, each one contating a prefilled description in comments


    1. main.R: Sets the current working directory and calls the following scripts: initialize.R, load_data.R, pull_data_from_DB.R, build.R, analyze.R.


    2. functions.R: All functions exist in this file. If they are too many, they can be separeted in several functions_XXX.R files.


    3. explore.R: Scripts chuncks for testing things or data exploration go here. It is the only file that is allowed to be messy. 


    4. initialize.R: This file loads all the packages,libraries and data needed regarding the workspace, loads the functions.R script and sets the global variables.


    5. load_data.R: This file loads all the csv/txt/xlsx/RDS/etc files needed and displays which files have been created/loaded in the workspace.


    6. pull_data_from_DB.R: This file uses dbplyr package to fetch filtered and grouped tables from the DB. Here client side operations should be keeped to bare minimum and no server side operations. All the new files created in the workspace should be displayed. These variables should be saved so the can be releoaded faster. A query_db boolean variable can be used to switch between DB fetch requests and reading from an RDS for faster data reload.


    7. build.R: Data wrangling with dplyr/tidyr/etc. All the magic happens here. Newly created files in workspace should be displayed. A build boolean variable can be used for data reload from RDS for faster data reload.


    8. analyze.R: Here does all the analysis steps. Results are reported in xlsx/csv media format in the designated folders.
    
 
    9. build_ppt.R: A template for powerpoint report using officer.


    10. prepare_markdown.R: Sets the current working directory, loads the data sets the markdown parameters and renders it.


    11. prepare_shiny.R: Sets the current working directory, loads the data sets the shiny parameters and runs the app.


    12. markdown_report.Rmd: A report template.


    13. shiny_report.Rmd: A Shiny app template.


---
  
### Execution:

1.  Put the files [project_initializer.R, DESCRIPTION, file_descriptions.csv]
in any folder with read/write privileges

2.  Run source(project_initializer.R) inside the project folder and
start coding!
