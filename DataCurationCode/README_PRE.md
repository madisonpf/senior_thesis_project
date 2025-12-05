# Data Curation Code

This folder contains the code used to convert the raw exported files into the final dataset used for analysis (not submitted due to FERPA restrictions - see data availability statement). This includes reading, merging, filtering, and anonymizing data.

The main steps in this workflow are:
- Combine and align raw input files
- Merge tables across terms and sources (Infoview exports, enrollment histories, etc.)
- Standardize field names and types
- Filter to analytical scope
- Include only individuals, time periods, and records used in analysis
- Remove extraneous terms, duplicate entries, and incomplete records
- Anonymize identifiers
- Export clean datasets
- Output final datasets in simple .csv or .tsv formats
- Preserve consistent column structure for later use

This code is available for submission and reviewer observation, but is requested not to be released for publication due to the locations of original/raw data. 

Structure: 
/Pre-processing : Contains merged pre-processing script
