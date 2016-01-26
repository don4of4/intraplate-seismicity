## intraplate-seismicity
An exploration of intraplate seismicity in the northeast USA.

####Table of contents:
* [Catalog Sources](#catalog-sources)
* [Magnitude Conversions](#magnitude-conversions)
* [Installation & Setup](#installation-setup)
* [Execution](#execution)
* [Updating](#updating)
* [Troubleshooting](#troubleshooting)
* [Directory Structure](#directory-structure)
* [How to use the software](#how-to-use-the-software)
  * [Stations Plot](#stations-plot)
  * [Earthquakes Plot](#earthquakes-plot)
  * [Density Plot (dbscan)](#density-plot)
  * [3D Plot](#3d-plot)
  * [Statistics](#statistics)

####Catalog Sources:
 - NEIC data set (includes historic and event until 2012): data.neic <- data/NEIC_HM_2014.csv
 - ANSS data set (all magnitudes from 2013-2015): data.anss <- data/ANSS_2013.csv
 - IRIS stations data: stations.iris <- data/all_stn_metadata_oct15"
 - ISC small magnitude data (1975-2012 with mag <2.5): data.small_mag <- data/small_mag.txt
 - ANF data import: data.anf <- data/ANF_06_15.txt
 - ISC arrivals data: data.arrivals <- data/isc_all_arrivals_no_restriction.txt

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

####How to use the software:
First, follow the instructions listed under “Installation & Setup” and “Execution.” Following these instructions are pertinent to using the dataset’s visual interface. An overview of the application and it’s functions are below:

*General Functions:*
The application allows the user to manipulate each visual representation through time. Simply utilize the “Time” slider at the top, or the “Set Time Range” feature under “Additional Functions.” This allows the user to manipulate the date-time range and thus influence each of the tabbed plots (listed below as items 1 thru 5).  

Under “Additional Functions”, the user can utilize the “Min Slider Value” in order to influence the “Time” slider’s start date. The user can select a rectangular dataset with regard to latitude and longitude by using the “Set Region” function. Lastly, the user can use the “Download Data” function in order to download the current dataset

1.	Stations Plot
The stations plot is a two-dimensional graph of seismometers over time. The seismometers are plotted in accordance with their longitude and latitude, and are placed on a map of the East coast U.S.A.

2.	Earthquakes Plot
The earthquakes plot is a two-dimensional graph of earthquakes over time. Each imported catalog is aggregated and presented in this graph in accordance with moment magnitude, which can be visualized through the associated color key.

3.	Density Plot (dbscan)
The density plot is a three-dimensional clustering of earthquakes over time. It aggregates earthquakes over latitude, longitude, and depth by use of DBSCAN, a density-based spatial clustering of applications with noise. The DBSCAN plot allows for manipulation of two variables: EPS (size of the epsiolon neighborhood), and MinPts (the minimum number of points in the EPS region). The user is welcome to manipulate the default settings in attempts to better cluster the data. More details can be found [here](https://cran.r-project.org/web/packages/dbscan/dbscan.pdf).

4.	3D Plot
The 3D plot is a representation of the clusters with regard to three-dimensional kernel density estimation. Using the kernel smoothing function in R, the plot allows for a graphical representation of each earthquake and then attempts to cluster. More details can be found [here](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/density.html) and [here](https://cran.r-project.org/web/packages/ks/ks.pdf).

5.	Statistics
The statistics graphs can be found by clicking on the "Statistics" tab then the special radio options below "Generate Statistics" under "Additional Functions." Each graph accurately displays an association between two factors, and some include a key with valuable information as well:
 - Cumulative Events & Magnitude is a distrubtion of frequency on magnitude, with the loess curve (span 0.7 and 0.6) listed, as well as the first derivative.
 - Cumulative Events & Time is a simple graph of cumulative number of events over time.
 - Stations vs. Year is a distrubution of the number of stations over time.
 - Num. of Events vs Depth accurately depicts the events that occured at given depths, clearly skewed right due to the significant number of recorded events without magnitude.
 - Num. of Events vs Magnitude lists the recorded events for each magnitude in one half increments from 0.5 to 6.

####How to add/update catalogs:
To add/update a catalog, follow the steps below:

1. Open `import.R` (for directions on how to do this, see the "Execution" section of the README.
2. Note the commented text, prefaced by a `#`. This will outline each existing dataset, their file location, and the labeling of the dataset's columns and source. To add a dataset, follow the syntax below:
  ```
data.NAME <- read.table("data/SRC", header = TRUE, sep = ",")
colnames(data.NAME) <- c("C1","C2","C3","C4*",...)
data.NAME$src <- 'LABEL'
data.NAME$declustered <- TRUE|FALSE
  ```
  - For NAME, choose a short variable name that will easily convey the dataset you are creating.
  - For SRC, you must first take the new CSV, TXT, or other file and place it in the /data/ folder. SRC should then be replaced with the name of the file. 
  - For C1, C2, C3, etc., you should choose column names for each column your SRC provides. If, for example, you are implementing station data for another region, you will most likely have "lat","lon","elev","depth", etc. Be sure each column label directly corresponds to the data in the given column. For example, if latitidue is listed in the third column, replace "C3" with "lat". 
  - Label the source of the data (choose more detailed syntax for LABEL).
  - Indicate whether the data is declustered (TRUE indicates YES, FALSE indicates NO). 
  - If necessary, follow the syntax below `# Date formatting` in order to standardize your new dataset's date-time information. Add your dataset to `data.neic$datetime <- ISOdatetime(..)` (utilizing the [ISOdatetime](https://stat.ethz.ch/R-manual/R-devel/library/base/html/ISOdatetime.html) conversion, or format according using a [POSIX](http://www.inside-r.org/r-doc/base/as.POSIXct) conversion. Feel free to use the existing conversions as a guide. 
  - If necessary, you might need to utilize the `#Magnitude conversion` section in order to standardize your data's magnitudes to Moment Magnitude.
  - That's it! Once you test and ensure yoru code runs smoothly, be sure to `git commit --all` and `git push` in order to save your work. You should do this via the command line (Terminal for Mac) or via the [Github Deskktop](https://desktop.github.com/) application.

3. To modify a dataset:
  - Find the dataset you wish to modify in the `/data/` directory. 
  - Add the new dataset to this folder. For example, add NEIC_HM_2016.csv into the directory. 
  - Go to the code (`import.R`) and modify the line of code `data.neic <- read.table("data/NEIC_HM_2016.csv", header = TRUE, sep = ",")`
    - Note that we are now pointing to NEIC_HM_2016.csv. 
  - Open the new CSV and verify that the column names directly associate with last year's data
    - i.e. `c("emw","lon","lat","depth","y","m","d", "h", "m.1", "s", "mwsig", "nstar","comment")`
  - If they do not, simply manipulate the above line in order to match the new column format. For example, if "emw" is now listed last, simply re-order in order to reflect the change.
  - That's it! Once you test and ensure yoru code runs smoothly, be sure to `git commit --all` and `git push` in order to save your work. You should do this via the command line (Terminal for Mac) or via the [Github Deskktop](https://desktop.github.com/) application.
