# Analysis Code

This directory contains the final R scripts used to generate the analytical outputs for the manuscript, including:
- Data processing and cleaning
- Variable construction (e.g., timing, course flags, demographic categories)
- Model fitting and inference
- Figure and table generation

The code is fully commented to document the data workflow, modeling decisions, and quality control checks.

Usage: 
*Important:* These scripts cannot be run without access to the institutional student records used in the analysis. The raw data are protected under FERPA and are not included in this repository. All derived datasets containing potentially identifying information have also been anonymized or aggregated prior to analysis, and are not shared. A Data Availability Statement in the manuscript explains these restrictions.

The code is provided to ensure transparency and reproducibility of:
- Statistical calculations
- Field course timing metrics
- Model outputs, effect estimates, and confidence intervals
- Supporting figures and tables used in the results

Items in directory: 
- 01_analysis.R - contains all analysis prep and statistical models, including some figures for basic distribution of data and reuslts
- 02_figures_and_supp_tables.R - contains code to create figures from main text using RDS objects from analysis script, Table 1 seen in MS, and all supplementary tables. 
