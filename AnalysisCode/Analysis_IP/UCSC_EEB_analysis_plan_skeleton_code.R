######################################### HOUSEKEEPING ##########################################

## LOAD PACKAGES
library(dplyr)
library(ggplot2)


#Pre-load in notes to-do: get access to Infoview, request access to csvs data pulls, ask Ali for finished data pulls for all years, download CSVs

## LOAD SEVEN DATA FILES 

## Student level data
student_id_key = read.csv("FILE", header = TRUE, sep = ",") #contains coded ID
degree = read.csv("FILE", header = TRUE, sep = ",") #contains coded ID, term of degree completion, degree type, current major, admit term, type of admit (first time frosh, transfer, etc.), applicant GPA
non-degree = read.csv("FILE", header = TRUE, sep = ",") #contains coded ID, term of degree completion, degree type, major, admit term, type of admit (first time frosh, transfer, etc.), applicant GPA

## Student - Quarter level data 
quarter_performance = read.csv("FILE", header = TRUE, sep = ",") #contains coded ID, cumulative GPA, term code, GPA of this quarter, credits earned this quarter, total credits, total credits attempted
quarter_info = read.csv("FILE", header = TRUE, sep = ",") #contains coded ID, race and ethnicity, domestic/international status, current gender

## Student - Class level data 
class_performance = read.csv("FILE", header = TRUE, sep = ",") #contains coded ID, official grade in course, catalog number of course, enrolled or dropped status, TA term code 

## Class level data 
offerings = read.csv("FILE", header = TRUE, sep = ",") #instructor name, number of enrolled students, title of course, catalog #, catalog # with section #, TA term code


########################################## DATA FILTERING ##############################################

#Filter offerings data table for BIOE 195
offerings_bioe_195 <- offerings %>%
  filter(grepl("BIOE", SE_Class_Title)) %>% 
  select(SE_Class_Title, SE_Subject_Catalog_Nbr, TA_Term_Code) #do we need catalog # or catalog # with section? do we even need to select by catalog # or can we subset by just title? 

# Filter student data for Marine Biology, EEB, and General Biology majors
filtered_degree <- degree %>%
  filter(current.major %in% c("Marine Biology", "EEB", "General Biology")) # Check real major names from Infoview datasets

# Add a BIOE 195 completion flag
class_performance <- class_performance %>%
  mutate(
    completed_bioe_195 = if_else(SE_Subject_Catalog_Nbr %in% offerings_bioe_courses$SE_Subject_Catalog_Nbr[grepl("BIOE 195", offerings_bioe_courses$SE_Class_Title)], "Yes", "No"), #create new column for completion of BIOE 195
    completed_bioe_20F = if_else(SE_Subject_Catalog_Nbr %in% offerings_bioe_courses$SE_Subject_Catalog_Nbr[grepl("BIOE 20F", offerings_bioe_courses$SE_Class_Title)], "Yes", "No"), #create new column for completion of 20F 
    completed_bioe_82 = if_else(SE_Subject_Catalog_Nbr %in% offerings_bioe_courses$SE_Subject_Catalog_Nbr[grepl("BIOE 82", offerings_bioe_courses$SE_Class_Title)], "Yes", "No"), #create new column for completion of 82
    completed_bioe_85 = if_else(SE_Subject_Catalog_Nbr %in% offerings_bioe_courses$SE_Subject_Catalog_Nbr[grepl("BIOE 85", offerings_bioe_courses$SE_Class_Title)], "Yes", "No"), #create new column for completion of 85
    completed_bioe_128L = if_else(SE_Subject_Catalog_Nbr %in% offerings_bioe_courses$SE_Subject_Catalog_Nbr[grepl("BIOE 128L", offerings_bioe_courses$SE_Class_Title)], "Yes", "No"), #create new column for completion of 182L
    completed_bioe_141L = if_else(SE_Subject_Catalog_Nbr %in% offerings_bioe_courses$SE_Subject_Catalog_Nbr[grepl("BIOE 141L", offerings_bioe_courses$SE_Class_Title)], "Yes", "No"), #create new column for completion of 141L
    # Add lower division field course course completion flag (grouping BIOE 20F, 82, 85)
    completed_low_div_fc = if_else(
      SE_Subject_Catalog_Nbr %in% offerings_bioe_courses$SE_Subject_Catalog_Nbr[grepl("BIOE 20F|BIOE 82|BIOE 85", offerings_bioe_courses$SE_Class_Title)],
      "Yes", "No"
    ),
    # Add upper division field course completion flag (grouping BIOE 128L, 141L)
    completed_high_div_fc = if_else(
      SE_Subject_Catalog_Nbr %in% offerings_bioe_courses$SE_Subject_Catalog_Nbr[grepl("BIOE 128L|BIOE 141L", offerings_bioe_courses$SE_Class_Title)],
      "Yes", "No")
)

#QUESTION: do we need only if BIOE 195 was taken, or do we need if it was dropped/not finished, or can we infer completion based on credits earned (do we also need another column for that?)


############################## MERGING AND PRE-PROCESSING DATA SHEETS #####################################

# 1. Merge class-level data with filtered offerings
cclass_data <- class_performance %>%
  left_join(offerings_bioe_195, by = c("cSE Subject Catalog Nbr" = "SE Subject Catalog Nbr", "TA Term Code"))

# 2. Merge quarter-level data for demographic and academic performance details
quarter_data <- quarter_performance %>%
  left_join(quarter_info, by = c("CodedID", "TA Term Code"))

# 3. Merge student-level data
student_data <- student_id_key %>%
  left_join(filtered_degree, by = "CodedID") #do we need to non-degree here? not sure what else it adds based on data dictionary

# 4. Combine all data
bioe_195_data <- class_data %>%
  left_join(quarter_data, by = c("CodedID", "TA Term Code")) %>%
  left_join(student_data, by = "CodedID")

# Select the required columns (Roxanne Q: Does this look ok or do we not need all of it?)
bioe_195_final <- bioe_195_data %>%
  select(
    CodedID, TA_Term_Code, PLNA_Plan_Descr, DG_Completion_Term_Code, DG_Degree_Short_Descr, UAD_Admit_Term_Code, UAD_First_Generation, UAD_Admit_Type_Descrip, 
    SE_Official_Grade, SE_Subject_Catalog_Nbr, SE_Subject_Catalog_Nbr-Section, CI_Instructor_Name, CS_Enrollment_Total, SE_Class_Title, 
    SE_Student_Enrollment_Status, ET_Ethnic_Group_Category, PD_Domestic/International-Current, PD_Gender-Current, SGC_Is_EOP, 
    TA_GPA-Cumulative, Term_GPA, Tot_Units_Earned, Tot_Units_Enrl, completed_bioe_195, completed_bioe_20F, completed_bioe_82, completed_bioe_85,
    completed_bioe_182L, completed_bioe_141L, completed_low_div_fc, completed_high_div_fc
  )

# View the final data set of BIOE 195 including student, term, and degree data 
head(bioe_195_final)

as.data.frame(bioe_195_final) #format data frame (contains Yes or No columns for senior thesis completion, field course completion [varying levels of detail], student demographics, and student success)


################################### PROCESSING FOR DUPLICATES/ERRORS ################################

###Quality check data to make sure one instructor for one student per term.
bioe_195_final <- bioe_195_final %>%
  group_by(CodedID, TA_Term_Code) %>%
  mutate(
    multiple_instructors = n_distinct(CI_Instructor_Name) > 1
  ) %>%
  ungroup()

# Logical check for any cases with multiple instructors
if (any(bioe_195_final$multiple_instructors)) {
  message("Duplicates exist")
  # Print the problematic cases
  print(bioe_195_final %>% filter(multiple_instructors))
} else {
  message("Unique instructor per student per term")
  
}

## Quality check for major filtering
table(final_data$current.major) #verify a count of students in each major 

#Remove duplicates if necessary 

#################################### DATA VISUALIZATIONS ############################################

## Make histograms of demographic data 
demographic_summary <- bioe_195_final %>%
  group_by(PD_Gender_Current, ET_Ethnic_Group_Category, PD_Domestic_International_Current, UAD_Admit_Type_Descr, UAD_First_Generation, SGC_is_EOP, completed_bioe_195) %>%
  summarize(count = n(), .groups = "drop")

# Plot histogram for gender
ggplot(demographic_summary %>% filter(!is.na(PD_Gender_Current)), 
       aes(x = PD_Gender_Current, y = count, fill = completed_bioe_195)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Senior Thesis Participation by Gender",
    x = "Gender",
    y = "Number of Students",
    fill = "Completed BIOE 195"
  ) +
  theme_minimal()

# Plot histogram for ethnicity
ggplot(demographic_summary %>% filter(!is.na(ET_Ethnic_Group_Category)), 
       aes(x = ET_Ethnic_Group_Category, y = count, fill = completed_bioe_195)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Senior Thesis Participation by Ethnicity",
    x = "Ethnicity",
    y = "Number of Students",
    fill = "Completed BIOE 195"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot histogram for domestic/international status
ggplot(demographic_summary %>% filter(!is.na(PD_Domestic_International_Current)), 
       aes(x = PD_Domestic_International_Current, y = count, fill = completed_bioe_195)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Senior Thesis Participation by Domestic/International Status",
    x = "Status",
    y = "Number of Students",
    fill = "Completed BIOE 195"
  ) +
  theme_minimal()

# Plot histogram for transfer status 
ggplot(demographic_summary %>% filter(!is.na(UAD_Admit_Type_Descr)), 
       aes(x = UAD_Admit_Type_Descr, y = count, fill = completed_bioe_195)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Senior Thesis Participation by Transfer Status",
    x = "Status",
    y = "Number of Students",
    fill = "Completed BIOE 195"
  ) +
  theme_minimal()

# Plot histogram for First Gen status 
ggplot(demographic_summary %>% filter(!is.na(UAD_First_Generation)), 
       aes(x = UAD_First_Generation, y = count, fill = completed_bioe_195)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Senior Thesis Participation by First Generation Status",
    x = "Status",
    y = "Number of Students",
    fill = "Completed BIOE 195"
  ) +
  theme_minimal()

# Plot histogram for EOP status
ggplot(demographic_summary %>% filter(!is.na(SGC_Is_EOP)), 
       aes(x = SGC_Is_EOP, y = count, fill = completed_bioe_195)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Senior Thesis Participation by EOP status",
    x = "Status",
    y = "Number of Students",
    fill = "Completed BIOE 195"
  ) +
  theme_minimal()

## Number of senior thesis students over time 
# Map quarters to names
quarter_names <- c(
  "0" = "Winter",
  "2" = "Spring",
  "4" = "Summer",
  "8" = "Fall"
)

# Parse TA_Term_Code into year and quarter
senior_thesis_time <- class_performance %>%
  filter(completed_bioe_195 == "Yes") %>%
  mutate(
    year = as.numeric(substr(TA_Term_Code, 1, 3)) + 2000, # First 3 digits + 2000
    quarter = quarter_names[substr(TA_Term_Code, 4, 4)],   # Map last digit to quarter name
    term_label = paste(quarter, year)                     # Combine into readable term
  ) %>%
  group_by(term_label) %>%
  summarize(
    count = n(),                             # Count of students per term
    sd = sd(count),                          # Standard deviation
    se = sd / sqrt(n()),                     # Standard error
    .groups = "drop"
  ) %>%
  arrange(year, quarter) # Ensure chronological order


# Plot the time-series data
ggplot(senior_thesis_time, aes(x = term_label, y = count)) +
  geom_line(color = "red", size = 1) +
  geom_point(aes(y = count), color = "black", size = 2, position = position_jitter(width = 0.2)) +
  geom_ribbon(aes(ymin = count - se, ymax = count + se), fill = "lightgrey", alpha = 0.3) +
  labs(
    title = "Number of Senior Thesis Students Over Time",
    x = "Term",
    y = "Number of Students",
    caption = "Error bars represent standard error (SE)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#################################### DATA ANALYSIS - SIGNIFICANCE TESTING ####################################

## GLM for demographic distribution based on senior thesis completion? or simple ANOVA depending on distribution? 

## Time effect --> linear mixed effect model controlling for demographic variability 
## --> change in proportion of y/n in each demographic category over time 

## Field course participation --> covariance testing to ensure field courses completion is not co-varying with demographics or senior thesis work
#--> then a linear regression or GLM depending on normality to check for significant differences in BIOE 195 participation as a factor of field course participation 
#--> determine the change in y/n depending on field course participation over time (visualization?)
