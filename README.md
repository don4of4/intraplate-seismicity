## intraplate-seismicity
An exploration of intraplate seismicity in the northeast USA.

####Catalog sources:
 - HM_2014 (flle: 2014_NEIC_declustered.c4; includes historic and event until 2012)
 - ANSS_2013 (file: anss; includes all mag events 2013 until jun 30, 2015)
 - Small_mag (file: small_mag_ISC_75_2012; includes events from 1975 to 2012 with mag < 2.5 or no magnitude determined)
 - ANF_06_15.txt 


####Magnitude Conversions:

Magnitude conversions where requred on data from ANSS_2013.  This was the methodology used.
 ![Table 3.3-1](docs/table3.3_1.png)

*Where the "dashed line" is the function:* `f(lat) = -0.45*lon + 3`
 

 
####Installation & Setup
 - Download and install R
 - Download and install RStudio
 - If Mac:  Download and install XQuartz
 -          Open Terminal
 -          Type 'git clone https://github.com/don4of4/intraplate-seismicity.git' without the quotes
 -          enter Github User and Password

####Execution:
 - Open RStudio
 - File/Open File...
 - Navigate to app.Rproj in the intraplate-seismicity folder (just added via Github)
 - Within RStudio, under Files, click import.R, highlight the entire contents of the file, and press Run
 - If needed, open server.R and press Run App
 
####Updating:
 - Open Terminal
 - Navigate to intraplate-seismicity
 - type 'git pull' without the quotes
 
####Troubleshooting
 - **Error: There is no package called ShinyRGL** → This error is indicative that XQuartz is not installed on Mac. If you just installed it, restart your R session. You should not recieve this error on PC.
 - **Error: object ‘dataset’ not found** → This error is indicative that you have not executed your import.R file. Stop the program execution, highlight contents of import.R, and execute the program. 

####Directory Structure:
```
intraplate-seismicity
├──  data/
│   ├──  //Data sources here
├──  docs/
│   ├──  ExceptionReport.docx
├──  app.Rproj
├──  import.R
├──  LICENSE
├──  README.md
├──  server.R
├──  stations_completeness_validation.R
├──  stations_date_validation.R
├──  ui.R
├──  util.R
```
