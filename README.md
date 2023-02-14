# Inflation Analysis
R scripts for reading in and analyzing CPI inflation data from the BLS's text website.

Written by: Mike Konczal

Last Updated: Feb 13, 2023.
 
File 1_load_cpi_data.R reads the latest data from the BLS website, cleans it up, attaches a weights file, and saves it.
This data is updated monthly, within minutes of the CPI numbers coming out. Weights are taken from the weights folder; currently weights are for 2023 values, and 2022 to previous years.

File 2_monthly_analysis.R is the current graphics I'm producing each month. They are written to a graphics folder.

File headline_overview.R is an overview of top-level variables.

File 3_median_CPI.R are dives into distribution of price changes.
 
File CPI_analysis_notepad.R is a series of one-off graphics I've made, or analysis I've done.

Old files and graphics are self-explanatory folders. Weights are where the weights are kept, manually downloaded, as well as specific items for distributional analysis.