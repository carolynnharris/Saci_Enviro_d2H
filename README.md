# Saci_Enviro_d2H
This repository holds data and supplementary source code for the manuscript, Harris et al. "Lipid hydrogen isotope compositions primarily reflect growth water in the model archaeon Sulfolobus acidocaldarius". (In press at Applied and Environmental Microbiology, special issue on Planetary Microbiology. Pre print availabe here: <https://www.biorxiv.org/content/10.1101/2024.10.24.620110v1.full>.) Repository is preserved on Zenodo here: [doi: 10.5281/zenodo.14743997](https://doi.org/10.5281/zenodo.14743997). Full author list: Carolynn M. Harris, Sebastian Kopf, Jeemin H. Rhim, Alec Cobban, Felix J. Elling, Xiahong Feng, Jamie McFarlin, Yuki Weber, Yujiao Zhang, Alice Zhou, Harpreet Batther, Ann Pearson, William D. Leavitt

In brief, this paper examines how growth rates, lipid profiles, and lipid/water H-isotope fractionation in a model hyperthermophilic archaeon vary in response to experimentally-manipulated growth conditions -- temperature, pH, aeration rate, electron acceptor flux, and electron donor flux. Growth rates were determined using opitcal density observations over time. Lipids were extracted from cell pellets using standard Bligh-Dyer extraction techniques. Tetraether lipids (GDGTs) were further derivitized to component biphytane chains via chemical ether cleavage. Biphytane profiles (identity and quantity) were analyzed via GC-MS and GC-FID. The H-isotope composition of individual biphytanes was analyzed via GC-Py-IRMS. 

The raw data is presented in the folder "00Data_Inputs". The R scripts will generate the tables and figures included in the manuscript and the supplemental information.

Please contact Carolynn Harris at carolynn.m.harris.gr@dartmouth.edu with any questions. 

## How do I run this code?
- Download and install R for your operating system
- Download and install RStudio for your operating system
- Download a zip file of this repository and decompress it in a directory of your choosing on your computer
- Navigate to the directory and open "Saci Enviro R Analysis.Rproj"  to start Rstudio and load this project's files
- Open the script(s) you would like to run. The scripts are numbered in the order they should be executed (e.g, 01, 02, 03, etc)
- Ensure that you have all of the required libraries installed by inspecting the Setup chunks 

## Folders
- 00_DataInputs: Contains all raw datasets needed to produce the tables and figures in this paper. There are subfolders for each dataset type: 
  - 01_Growth_Curves - files are named after the researcher and year the experiment was conducted  
  - 02_BP_Iso_Data - file contains all lipid and isotope data compiled by the lead author. The metadata tab provides units and description for all data columns.  
  - 03_GCFID_Chromatogram - a representative chromatogram from S. acidocaldarius containing all BP moieties  
  - 04_Ring_Difference - summary of published ring difference data 
  - 05_Rhim et al., 2024 - summary of published fractionation values compiled by Rhim et al., 2024 
  - 06_Dirghangi and Pagani 2013 - Fractionation and growth data from a study on H. marismortui 
- 00_Experiment Dataframes: Populated with .RDS and .csv summary files for each experiment, produced by "00 Data Calcs & Experiment Subsets.R" script
- 01_SummaryTables: Populated with summary tables CSV files after running "01 Sumary Tables.R" script
- 02_SummaryFigs: Populated with figures (PNG files) after running the other scripts (see below for details)

## Scripts
- 00 Data Calcs & Experiment Subsets.R: Performs initial data cleaning, all calculations (e.g. Ring Indices, lipid/water fractionation calculations, weighted mean calculations, etc.), and subsets the data into environmental experiments (presented in the main text) and the 2H-labeled water experiments (presented in the Supplementary Materials).
- 01 Sumary Tables.R: Generates Tables 1, 2, S1, S2 which summaryize growth metrics, lipid distributions, isotope values, and fractionation values for each level of each experimental treatment
- 02 Growth Curves Figs.R: Generates the growth curves in Fig. S1
- 03 Experiment Summary Plot.R: Generates the summary overview figure, Fig. 2
- 04 Fractionation vs Doubling Time.R: Generates the plot of doubling time vs. weighted mean lipid/water fractionation, Fig. 5
- 05 Ring Difference vs Environmental Condition.R: Generates the plot of environmental condition vs. mean ring difference, Fig. S4
- 06 Ring Difference vs Ring Number.R: Generates the plot of BP ring number vs. mean ring difference, Fig. 3
- 07 Correlation Matrices.R: Generates the summary correlation matrices, Fig. 4
- 08: 08 EpsLW Density Curves by Domain.R: Generates the plot of probability density functions for lipid/water fractionation values for each Domain, Fig. 6
- 09 RI_BP vs RI_GDGT.R: Generates the plot of GDGT Ring Index vs. BP Ring Index, Fig. S2 
- 10 Fractionation vs Environmental Condition_allBPs.R: Generates the plot of environmental condition vs. lipid/water fractionation for each individual BP, Fig. S3
- 11 Dirghangi and Pagani 2013.R: Generates the plot of doubling time vs. fractionation for H. mari (data from Dirghangi and Pagani 2013), Fig S5
- 12 Saci GC-FID Chromatogram.R: Generates the plot of a representative chromatogram for S. aci, Fig. 1
