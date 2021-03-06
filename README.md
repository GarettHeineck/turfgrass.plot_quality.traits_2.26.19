# turfgrass.plot_quality.traits_2.26.19

## Project information
### 1. This project demonstrates how to use R to quantify several traits in turf type perennial ryegrass.  
   1. Three perennial ryegrass plots were imaged in the field under ambient light conditions in August 2018.
   1. Plots were seeded in 2017 in Minnesota at the UMN St.Paul research farm.
   1. Second year perennial ryegrass often succombs to winterkill and subsequent crabgrass invasion in the second year.
   1. Because perennial ryegrass is a prolific seed producer second year plots will often have decreased quality due to stemminess.
   1. These images were taken with a simple DSLR camera and are saved as JPGs in the 'original images' folder.
### 2. A metadata sheet is included in the 'results' folder.  
   1. The data sheet contains important information to validate the computer output.
   1. Variables were collected manually on turfgrass quality, stemminess, and crabgrass infestation.
### 3. Several training data csv files are included in the 'training data' folder.  
   1. Collectively, these mixes (csv files) make up the training palette for this project.
   1. The training palette is used to fit random forest models.
   1. The training palette can be expanded or shrunk by adding or subtracting mixes.
   1. New mixes can be made easily in ImageJ (SEE MODULE ON MAKING TRAINING DATA).
   1. Mixes can be subtracted by removing the files after downloading or using R.


## Setup
1. Start by downloading the ZIP file with all documents by clicking the "clone or download" button.
1. Save these documents in a folder anywhere on your hard drive called 'turfgrass.plot_quality.traits_2.26.19'.
1. NOTE: if you do not use this file name it will take a little more work to run the R script.
1. Open the R file in R studio.
1. Go through each required package and download or update packages if needed.
1. Update R if you have not done so recently.
1. In the R file go to line 32 and change the object file path to wherever your 'turfgrass.plot_quality.traits_2.26.19' folder is located.
1. The script should run and conduct the following tasks:
   1. Create new folders within 'turfgrass.plot_quality.traits_2.26.19'.
   1. Each folder contains a step in the pipeline (S1-S7).
   1. Build three random forest models to detect foreground and crown rust.
   1. Operate image analysis using the models and EBImage functions.
   1. Output and save a summary data file.
   1. Conduct a few basic visualizations in ggplot.
1. Currently the final processing step (lines 193-197) have been commented out
   1. Some processor cannot handle the watershed transformation and so by default it is not included (folder S7 will be empty).
   1. To incorporate that step remove the hash marks from lines 193-197 and 206-207.
   1. Cluster analysis is interesting, but not necessary for this particular trait. 
1. This script will take about 10-15 minutes to run (13.3 min with a 2014 MacBook Pro 16G RAM macOS High Sierra).
