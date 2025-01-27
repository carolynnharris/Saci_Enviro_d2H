# Saci_Enviro_d2H
Supplementary Materials for Harris et al. "Lipid hydrogen isotope compositions primarily reflect growth water in the model archaeon Sulfolobus acidocaldarius". bioRxiv (in review) <https://www.biorxiv.org/content/10.1101/2024.10.24.620110v1.full>. This repository holds data and supplementary source code for the manuscript. 

The raw data is presented in the folder "00Data_Inputs". The R scripts will generate the tables and figures included in the manuscript and the supplement.
The main dataset for this paper is also hosted on zenodo at: 

Please contact Carolynn Harris at carolynn.m.harris.gr@dartmouth.edu with any questions. 

## How do I run this code?
- Download and install R for your operating system
- Download and install RStudio for your operating system
- Download a zip file of this repository and decompress it in a directory of your choosing on your computer
- Navigate to the directory and open "Saci Enviro R Analysis.Rproj"  to start Rstudio and load this project's files
- Open the script(s) you would like to run. The scripts are numbered in the order they should be executed (e.g, 01, 02, 03, etc). 
- Ensure that you have all of the required libraries installed by inspecting the Setup chunks. 

## Folders
- 00_DataInputs: Contains all raw data needed to produce the tables and figures in this paper
- 00_Experiment Dataframes: Populated with .RDS and .csv summary files for each experiment after running "00 Data Calcs & Experiment Subsets.R" script
- 01_SummaryTables: Populated with summary tables (.csv) files after running "01 Sumary Tables.R" script
- 02_SummaryFigs: Populated with figures (.png) after running the other scripts (see below for details)

## Scripts
- 00 Data Calcs & Experiment Subsets.R: Performs initial data cleaning, all calculations (e.g. Ring Indices, lipid/water fractionation calculations, weighted mean calculations, etc.), and subsets the data into environmental experiments (presented in the main text) and the 2H-labeled water experiments (presented in the Supplementary Materials).
- 01 Sumary Tables.R: Generates Tables 1, 2, S1, S2
- 02 Growth Curves Figs.R: Generates the growth curves in Fig. S1
- 03 Enviro exp master plot.R: Generates the summary overview figure, Fig. 2
- 04 Wt eps vs T_D.R: Generates the plot of doubling time vs. weighted mean lipid/water fractionation, Fig. 5
- 05 Ring Difference.R: Generates the plot of environmental condition vs. mean ring difference, Fig. S4
- 06 Ring diff vs BP.R: Generates the plot of BP ring number vs. mean ring difference, Fig. 3
- 07 Correlograms.R: Generates the summary correlation matrices, Fig. 4
- 08: PDFs of EpsLW by Domain_Rhim et al.R: Generates the plot of density functions for lipid/water fractionation values for each Domain, Fig. 6
- 09 RI_BP vs RI_GDGT.R: Generates the plot of GDGT Ring Index vs. BP Ring Index, Fig. S2 
- 10 Eps_vs_enviro_indivBPs.R: Generates the plot of environmental condition vs. lipid/water fractionation for each individual BP, Fig. S3
- 11 Dirghangi and Pagani 2013.R: Generates the plot of doubling time vs. fractionation for H. mari (data from Dirghangi and Pagani 2013), Fig S5
- 12 Saci Chromatogram.R: Generates the plot of a representative chromatogram for S. aci, Fig. 1
