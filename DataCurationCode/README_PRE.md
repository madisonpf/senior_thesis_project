# Data Curation Code

This is where you store the code that converts the *RawData* to your *DryadData*. This file is also to store data pre-processing code (before final analysis) used for merging Infoview files and prepping data for analysis.

The main steps here are: 

1. Combine the tables and fields you need
2. Filter the data to only the individuals, dates, etc. used in analysis
3. Check and make sure that IDs are anonymous 
   
Make sure to keep all possible files in basic csv (or tsv) format for easy access.

The three folders correspond to three uses:
*Pre-processing code* is intended for pre-processing code and can be less polished (not as many comments and in progress) *private*
