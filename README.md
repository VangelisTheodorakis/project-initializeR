#project-initializeR

##Scaffolding for large R Projects

###Description: 

Just a try for some workflow/structure setup in order to cope with the
lack of scaffolding tools in R for big projects.
Description: 

Just a try for some workflow/structure setup in order to cope with the
lack of scaffolding tools in R for big projects.

---

###Project Scaffolding:
    
..* Project:
..1. info:          pdfs, powerpoints, docs, etc anything that is not affiliated with the scripts
..2. data-input:    data that will be used by the scripts but is not generated by them
..3. data-output:   data that will be generated by the scripts but is not in a form of proper report
..4. reports:       only files that can be shown to someone else in a proper form
..5. R:             all the R scripts, each one contating a prefilled description in comments
---
  
###Execution:

1.  Put the files [project_initializer.R, DESCRIPTION, file_descriptions.csv]
in any file with read/write privileges

2.  Run source("project_initializer.R") inside the project folder and
start coding!