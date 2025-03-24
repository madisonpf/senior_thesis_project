######################################### HOUSEKEEPING ##########################################

## LOAD PACKAGES
library(dplyr)
library(tidyr)
library(ggplot2)

## LOAD SEVEN DATA FILES 

## Student level data
degree = read.csv("./PhD Documents/Senior Thesis Project/data/Degree.csv", header = TRUE, sep = ",") #contains coded ID, term of degree completion, degree type, current major, admit term, type of admit (first time frosh, transfer, etc.), applicant GPA
non_degree = read.csv("./PhD Documents/Senior Thesis Project/data/Non-Degree.csv", header = TRUE, sep = ",") #contains coded ID, term of degree completion, degree type, major, admit term, type of admit (first time frosh, transfer, etc.), applicant GPA

## Student - Quarter level data 
quarter_performance = read.csv("./PhD Documents/Senior Thesis Project/data/QuarterPerformance.csv", header = TRUE, sep = ",") #contains coded ID, cumulative GPA, term code, GPA of this quarter, credits earned this quarter, total credits, total credits attempted
quarter_info = read.csv("./PhD Documents/Senior Thesis Project/data/QuarterInfo_v2_feb5.csv", header = TRUE, sep = ",") #contains coded ID, race and ethnicity, domestic/international status, current gender

## Student - Class level data 
class_performance = read.csv("./PhD Documents/Senior Thesis Project/data/ClassPerformance.csv", header = TRUE, sep = ",") #contains coded ID, official grade in course, catalog number of course, enrolled or dropped status, TA term code 

## Class level data 
offerings = read.csv("./PhD Documents/Senior Thesis Project/data/Offerings.csv", header = TRUE, sep = ",") #instructor name, number of enrolled students, title of course, catalog #, catalog # with section #, TA term code


########################################## DATA FILTERING ##############################################

###Check how many occurences of the courses there are 
courses <- c("BIOE 195", "BIOE 20F", "BIOE 82", "BIOE 85", "BIOE 128L", "BIOE 141L", "BIOE 127L")
course_counts <- sapply(courses, function(course) sum(class_performance$SE.Subject.Catalog.Nbr == course, na.rm = TRUE))
print(course_counts)

# Create a dataframe for visualization
bioe_courses <- data.frame(
  Course = c("BIOE 195", "BIOE 20F", "BIOE 82", "BIOE 85", "BIOE 128L", "BIOE 141L", "BIOE 127L"),
  Students = c(135, 384, 1073, 133, 166, 169, 974)
)

# Plot distribution of BIOE 195 Participation over time
ggplot(bioe_courses, aes(x = reorder(Course, Students), y = Students, fill = Course)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  
  labs(
    x = "Course",
    y = "Number of Students"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "BrBG")  

 # Add completion flags
final_class_performance <- class_performance %>%
  mutate(
    completed_bioe_195 = if_else(SE.Subject.Catalog.Nbr == "BIOE 195", "Yes", "No"),
    completed_bioe_20F = if_else(SE.Subject.Catalog.Nbr == "BIOE 20F", "Yes", "No"),
    completed_bioe_82 = if_else(SE.Subject.Catalog.Nbr == "BIOE 82", "Yes", "No"),
    completed_bioe_85 = if_else(SE.Subject.Catalog.Nbr == "BIOE 85", "Yes", "No"),
    completed_bioe_128L = if_else(SE.Subject.Catalog.Nbr == "BIOE 128L", "Yes", "No"),
    completed_bioe_141L = if_else(SE.Subject.Catalog.Nbr == "BIOE 141L", "Yes", "No"),
    completed_bioe_127L = if_else(SE.Subject.Catalog.Nbr == "BIOE 127L", "Yes", "No"),
    
    # Flag for completing any lower division field course (BIOE 20F, 82, 85)
    completed_low_div_fc = if_else(SE.Subject.Catalog.Nbr %in% c("BIOE 20F", "BIOE 82", "BIOE 85"), "Yes", "No"),
    
    # Flag for completing any upper division field course (BIOE 128L, 141L)
    completed_high_div_fc = if_else(SE.Subject.Catalog.Nbr %in% c("BIOE 128L", "BIOE 141L", "BIOE 127L"), "Yes", "No")
  )

#Note on future work: Start w/ original cohort --> who finished and who didn't --> for those who completed, start w/ original set of analysis, then linear regression of dropped students 

############################## MERGING AND PRE-PROCESSING DATA SHEETS #####################################

# Remove rows where TA.Term.Code is "-"
quarter_perf_clean <- quarter_performance %>%
  filter(TA.Term.Code != " -") 
# Remove rows where TA.Term.Code is "-"
quarter_info_clean <- quarter_info %>%
  filter(TA.Term.Code != " -")  

#1. Merge quarter info and quarter performance
quarter_data <- left_join(quarter_info_clean, quarter_perf_clean,
                        by = c("CodedID", "TA.Term.Code"))

#2. Merge degree info with merged quarter 
degree_quarter <- left_join(degree, quarter_data,
  by = c("CodedID", "PLNA.Plan.Descr")
)

#Adjust formatting
degree_quarter <- degree_quarter %>%
  mutate(TA.Term.Code = as.character(TA.Term.Code)) #make formatting consistent

final_class_performance <- final_class_performance %>%
  mutate(TA.Term.Code = as.character(TA.Term.Code)) 


#3. Merge degree + quarter with class performance by all shared files
bioe_195_final <- left_join(degree_quarter, final_class_performance, by = c("CodedID", "TA.Term.Code"))

#Make offerings consistent
offerings <- offerings %>%
  mutate(TA.Term.Code = as.character(TA.Term.Code))

#3A. Merge offerings for now but use bioe_195_final for analysis because of issues with TA.Term.Code 
add_off <- left_join(bioe_195_final, offerings, by = c("SE.Subject.Catalog.Nbr", "TA.Term.Code"))

# View the final data set of BIOE 195 including student, term, and degree data 
head(bioe_195_final)

as.data.frame(bioe_195_final) 

write.csv(bioe_195_final, "./PHD Documents/Senior Thesis Project/data/bioe_195_final.csv")

################################### PROCESSING FOR DUPLICATES/ERRORS ################################

###ONLY FOR OFFERINGS FILE -- Quality check data to make sure one instructor for one student per term - multiple thesis instructors? (pivot wider/longer?) 
add_off <- add_off %>%
  group_by(CodedID, TA.Term.Code) %>%
  mutate(
   multiple_instructors = n_distinct(CI.Instructor.Name) > 1
  ) %>%
  ungroup()

which(bioe_195_final == "BIOE BS", arr.ind = TRUE)

bioe_students <- bioe_195_final %>%
  filter(PLNA.Plan.Descr %in% c("Ecology and Evolution BS", "Marine Biology BS", "Biology BS", "Plant Sciences BS"))


# Logical check for any cases with multiple instructors from the OFFERINGS file
if (any(add_off$multiple_instructors)) {
  message("Duplicates exist")
  # Print the problematic cases
  print(add_off %>% filter(multiple_instructors))
} else {
  message("Unique instructor per student per term")
  
}

## Quality check for major filtering
table(bioe_students$PLNA.Plan.Descr) #verify a count of students in each major 


######################################### BASIC DISTRIBUTIONS ############################################
#Filter by BIOE 195 from EEB, MB, B, and PS majors 
bioe_195_final <- bioe_students %>%
  filter(SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  group_by(CodedID) %>%
  filter(TA.Term.Code == max(TA.Term.Code)) %>%  # Keep only the latest term per student
  ungroup()

## Number of senior thesis students over time 
# Map quarters to names
quarter_names <- c(
  "0" = "Winter",
  "2" = "Spring",
  "4" = "Summer",
  "8" = "Fall"
)

# Parse TA.Term.Code into year and quarter
senior_thesis_time <- class_performance %>%
  filter(completed_bioe_195 == "Yes") %>%
  mutate(
    year = as.numeric(substr(TA.Term.Code, 1, 3)) + 2000, # Extract year
    quarter = as.numeric(substr(TA.Term.Code, 4, 4)),     # Extract quarter as number
    numeric_term = year + quarter / 10,                   # Create numeric term for sorting
    term_label = case_when(                                # Convert quarter numbers to names
      quarter == 8 ~ paste("Fall", year),
      quarter == 0 ~ paste("Winter", year),
      quarter == 2 ~ paste("Spring", year),
      quarter == 4 ~ paste("Summer", year),
      TRUE ~ paste("Unknown", year) # Just in case there are unexpected values
    )
  ) %>%
  group_by(numeric_term, term_label) %>%
  summarize(
    count = n(),                             # Count students per term
    sd = ifelse(n() > 1, sd(count), 0),      # Standard deviation, avoid NA when n=1
    se = ifelse(n() > 1, sd / sqrt(n()), 0), # Standard error, avoid NA when n=1
    .groups = "drop"
  ) %>%
  arrange(numeric_term)

# Plot the time-series data
ggplot(senior_thesis_time, aes(x = factor(term_label, levels = unique(term_label)), y = count)) +
  geom_point(color = "#2C3E50", size = 3, alpha = 0.8, position = position_jitter(width = 0.2)) +  # Dark blue points
  labs(
    x = "Term",
    y = "Number of Students",
  ) +
  theme_minimal(base_size = 14) +  # Increase font size for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "#34495E", size = 12),  # Rotate and adjust labels
    axis.text.y = element_text(color = "#34495E", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16, color = "#2C3E50"),
    plot.subtitle = element_text(size = 14, color = "#566573"),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),  # Softer grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


###################################### STUDENT PARTICIPATION OVER TIME #################################################

# Total number of students by year (including all students)
total_students_per_year <- bioe_students %>%
  group_by(TA.Term.Code) %>%
  summarise(total_students = n(), .groups = "drop")

# Number of students who took BIOE 195 by year
students_taken_bioe195_per_year <- bioe_students %>%
  filter(SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  group_by(TA.Term.Code) %>%
  summarise(bioe_195_students = n(), .groups = "drop")

# Merge both data frames by year
students_summary <- left_join(total_students_per_year, students_taken_bioe195_per_year, by = "TA.Term.Code")

# Calculate proportion of BIOE 195 students relative to the total students for each year
students_summary <- students_summary %>%
  mutate(proportion_bioe195 = bioe_195_students / total_students)

students_summary <- students_summary %>%
  mutate(year = as.numeric(TA.Term.Code)) %>%
  arrange(TA.Term.Code)

# Plot the proportion of BIOE 195 students relative to the total students for each year
ggplot(students_summary, aes(x = year, y = proportion_bioe195)) +
  geom_point(color = "#A52A2A", size = 3, alpha = 0.7) +  # Brown points
  geom_smooth(method = "lm", color = "#1F77B4", size = 1.2, linetype = "solid") +  # Blue linear regression line
  labs(
    x = "Year",
    y = "Proportion of BIOE 195 Students",
    caption = "Proportion calculated as students who took BIOE 195 / Total students"
  ) +
  theme_minimal(base_size = 14) +  # Increase font size for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "#34495E", size = 12),  # Rotate x-axis labels
    axis.text.y = element_text(color = "#34495E", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16, color = "#2C3E50"),
    plot.subtitle = element_text(size = 14, color = "#566573"),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),  # Softer grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


####################################### DEMOGRAPHICS #######################################################################
# Group by senior thesis completion and demographic group, then count students
demographic_summary <- bioe_students %>%
  filter(!is.na(PD.Gender...Current) & !is.na(ET.Ethnic.Group.Category) & 
           !is.na(PD.Domestic.International...Current) & !is.na(UAD.Admit.Type.Descr) & 
           !is.na(UAD.First.Generation) & !is.na(SGC.Is.EOP) & !is.na(completed_bioe_195)) %>%
  group_by(PD.Gender...Current, ET.Ethnic.Group.Category, PD.Domestic.International...Current, 
           UAD.Admit.Type.Descr, UAD.First.Generation, SGC.Is.EOP, completed_bioe_195, TA.Term.Code) %>%
  summarise(count = n(), .groups = "drop")

# Load patchwork for combining plots
library(patchwork)

# Define a color palette for categorical data
color_palette <- scale_fill_brewer(palette = "Spectral") #Need to change to match demographic v. time plots 

### MAKE STACKED BAR PLOTS FOR COMPLETITION 
# Gender Plot
gender_bar_plot <- ggplot(demographic_summary, aes(x = completed_bioe_195, y = count, fill = PD.Gender...Current)) +
  geom_bar(stat = "identity", position = "fill") +
  color_palette +
  labs(title = "Senior Thesis Completion by Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ethnicity Plot
ethnicity_bar_plot <- ggplot(demographic_summary, aes(x = completed_bioe_195, y = count, fill = ET.Ethnic.Group.Category)) +
  geom_bar(stat = "identity", position = "fill") +
  color_palette +
  labs(title = "Senior Thesis Completion by Ethnicity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# First-Gen Plot
first_gen_bar_plot <- ggplot(demographic_summary, aes(x = completed_bioe_195, y = count, fill = UAD.First.Generation)) +
  geom_bar(stat = "identity", position = "fill") +
  color_palette +
  labs(title = "Senior Thesis Completion by First-Generation Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# International Plot
international_bar_plot <- ggplot(demographic_summary, aes(x = completed_bioe_195, y = count, fill = PD.Domestic.International...Current)) +
  geom_bar(stat = "identity", position = "fill") +
  color_palette +
  labs(title = "Senior Thesis Completion by International Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transfer Status Plot
transfer_bar_plot <- ggplot(demographic_summary, aes(x = completed_bioe_195, y = count, fill = UAD.Admit.Type.Descr)) +
  geom_bar(stat = "identity", position = "fill") +
  color_palette +
  labs(title = "Senior Thesis Completion by Transfer Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EOP Status Plot
eop_bar_plot <- ggplot(demographic_summary, aes(x = completed_bioe_195, y = count, fill = SGC.Is.EOP)) +
  geom_bar(stat = "identity", position = "fill") +
  color_palette +
  labs(title = "Senior Thesis Completion by EOP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine all plots into one figure
combined_plot <- gender_bar_plot + ethnicity_bar_plot + first_gen_bar_plot + international_bar_plot + transfer_bar_plot + eop_bar_plot +
  plot_layout(ncol = 2)

# Display the combined plot
combined_plot

### DEMOGRAPHICS OVER TIME ###
#Group by year and make it so they appear as 2008, 2009, etc. vs. in TA.Term.Code style
bioe_students <- bioe_students %>%
  mutate(
    TA.Term.Code = as.character(TA.Term.Code),  # Convert to character if it's numeric
    Year = as.integer(substr(TA.Term.Code, 1, 3)) + 1800  # Add 2000 to the extracted 3 digits
  )

#EOP Completion calculation for count 
eop_completion <- bioe_students %>%
  filter(SGC.Is.EOP == "Y") %>%
  group_by(Year) %>%
  summarise(
    total_eop_students = n(),
    completed_bioe_195 = sum(completed_bioe_195 == "Yes", na.rm = TRUE),
#    proportion_completed = completed_bioe_195 / total_eop_students,
    .groups = "drop"
  ) %>%
  filter(!is.na(Year))

#Non-EOP calculation for count
non_eop_completion <- bioe_students %>%
  filter(SGC.Is.EOP == "N") %>%
  group_by(Year) %>%
  summarise(
    total_noneop_students = n(),
    none_completed_bioe_195 = sum(completed_bioe_195 == "Yes", na.rm = TRUE),
#    n_proportion_completed = completed_bioe_195 / total_noneop_students,
    .groups = "drop"
  ) %>%
  filter(!is.na(Year))

#Plot distributions by year
eop_plot <- ggplot() +
  # Plot for EOP Students
  geom_line(data = eop_completion, aes(x = factor(Year), y = completed_bioe_195, color = "EOP", group = 1), size = 1.5) +
  geom_point(data = eop_completion, aes(x = factor(Year), y = completed_bioe_195, color = "EOP"), size = 4) +
  
  # Plot for Non-EOP Students
  geom_line(data = non_eop_completion, aes(x = factor(Year), y = none_completed_bioe_195, color = "Non-EOP", group = 1), size = 1.5) +
  geom_point(data = non_eop_completion, aes(x = factor(Year), y = none_completed_bioe_195, color = "Non-EOP"), size = 4, group = 1) +
  
  labs(
    x = "Year",
    y = "Number of Students Completing BIOE 195"
  ) +
  scale_color_manual(values = c("EOP" = "#231942", "Non-EOP" = "#e0b1cb")) +  
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    axis.title.y = element_blank()
  )


###First generation calculations
first_gen_completion <- bioe_students %>%
  filter(UAD.First.Generation == "First Generation") %>%
  group_by(Year) %>%
  summarise(
    total_f_gen_students = n(),
    fg_completed_bioe_195 = sum(completed_bioe_195 == "Yes", na.rm = TRUE),
#    fg_proportion_completed = completed_bioe_195 / total_f_gen_students,
    .groups = "drop"
  ) %>%
  filter(!is.na(Year))

non_first_gen_completion <- bioe_students %>%
  filter(UAD.First.Generation == "Not First Generation") %>%
  group_by(Year) %>%
  summarise(
    total_nonfg_students = n(),
    nfg_completed_bioe_195 = sum(completed_bioe_195 == "Yes", na.rm = TRUE),
#    nfg_proportion_completed = completed_bioe_195 / total_nonfg_students,
    .groups = "drop"
  ) %>%
  filter(!is.na(Year))

#Plot by year 
first_gen_plot <- ggplot() +
  # Plot for First Generation Students
  geom_line(data = first_gen_completion, aes(x = factor(Year), y = fg_completed_bioe_195, color = "First Generation", group = 1), size = 1.5) +
  geom_point(data = first_gen_completion, aes(x = factor(Year), y = fg_completed_bioe_195, color = "First Generation"), size = 4) +
  
  # Plot for Non-First Generation Students
  geom_line(data = non_first_gen_completion, aes(x = factor(Year), y = nfg_completed_bioe_195, color = "Not First Generation", group = 1), size = 1.5) +
  geom_point(data = non_first_gen_completion, aes(x = factor(Year), y = nfg_completed_bioe_195, color = "Not First Generation"), size = 4) +
  
  labs(
    x = "Year",
    y = "Number of Students Completing BIOE 195"
  ) +
  scale_color_manual(values = c("First Generation" = "#184e77", "Not First Generation" = "#52b69a")) + 
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    axis.title.y = element_blank()
  ) 


###Domestic v. International calculations
inter_completion <- bioe_students %>%
  filter(PD.Domestic.International...Current == "International") %>%
  group_by(Year) %>%
  summarise(
    total_inter_students = n(),
    inter_completed_bioe_195 = sum(completed_bioe_195 == "Yes", na.rm = TRUE),
#    proportion_completed = completed_bioe_195 / total_f_gen_students,
    .groups = "drop"
  ) %>%
  filter(!is.na(Year))  # Remove rows with NA years

dom_completion <- bioe_students %>%
  filter(PD.Domestic.International...Current == "Domestic") %>%
  group_by(Year) %>%
  summarise(
    total_dom_students = n(),
    dom_completed_bioe_195 = sum(completed_bioe_195 == "Yes", na.rm = TRUE),
#    proportion_completed = completed_bioe_195 / total_f_gen_students,
    .groups = "drop"
  ) %>%
  filter(!is.na(Year))  # Remove rows with NA years

# Plot with Year on the x-axis
international_stat_plot <- ggplot() +
  # Plot for First Generation Students
  geom_line(data = dom_completion, aes(x = factor(Year), y = dom_completed_bioe_195, color = "Domestic", group = 1), size = 1.5) +
  geom_point(data = dom_completion, aes(x = factor(Year), y = dom_completed_bioe_195, color = "Domestic"), size = 4) +
  
  # Plot for Non-First Generation Students
  geom_line(data = inter_completion, aes(x = factor(Year), y = inter_completed_bioe_195, color = "International", group = 1), size = 1.5) +
  geom_point(data = inter_completion, aes(x = factor(Year), y = inter_completed_bioe_195, color = "International"), size = 4) +
  
  labs(
    x = "Year",
    y = "Number of Students Completing BIOE 195"
  ) +
  scale_color_manual(values = c("Domestic" = "#d76a03", "International" = "#fabc3c")) + 
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )  


###Gender calculations
process_gender_data <- function(data, gender, column_prefix) {
  data %>%
    filter(PD.Gender...Current == gender) %>%
    group_by(Year) %>%
    summarise(
      !!paste0("total_", column_prefix, "_students") := n(),
      !!paste0(column_prefix, "_completed_bioe_195") := sum(completed_bioe_195 == "Yes", na.rm = TRUE),
      gender = gender,  # Assign gender label
      .groups = "drop"
    ) %>%
    filter(!is.na(Year))
}

# Applying Function for Each Gender
female_completion <- process_gender_data(bioe_students, "F", "f")
male_completion <- process_gender_data(bioe_students, "M", "m")
x_completion <- process_gender_data(bioe_students, "X", "x")
u_completion <- process_gender_data(bioe_students, "U", "u")

# Plot
gender_plot <- ggplot() +
  geom_line(data = female_completion, aes(x = factor(Year), y = f_completed_bioe_195, color = "Female", group = 1), size = 1.5) +
  geom_point(data = female_completion, aes(x = factor(Year), y = f_completed_bioe_195, color = "Female"), size = 4) +
  
  geom_line(data = male_completion, aes(x = factor(Year), y = m_completed_bioe_195, color = "Male", group = 1), size = 1.5) +
  geom_point(data = male_completion, aes(x = factor(Year), y = m_completed_bioe_195, color = "Male"), size = 4) +
  
  geom_line(data = x_completion, aes(x = factor(Year), y = x_completed_bioe_195, color = "X", group = 1), size = 1.5) +
  geom_point(data = x_completion, aes(x = factor(Year), y = x_completed_bioe_195, color = "X"), size = 4) +
  
  geom_line(data = u_completion, aes(x = factor(Year), y = u_completed_bioe_195, color = "U", group = 1), size = 1.5) +
  geom_point(data = u_completion, aes(x = factor(Year), y = u_completed_bioe_195, color = "U"), size = 4) +
  
  labs(
    x = "Year",
    y = "Number of Students Completing BIOE 195"
  ) +
  scale_color_manual(values = c("Female" = "#bbd0ff", "Male" = "#58148e", "X" = "#c18cf9", "U" = "#ffc4ff")) +  # Custom colors
  theme_minimal() +  # Minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    legend.title = element_blank()
  )

###Ethnicity x Completition
process_ethnicity_data <- function(data, ethnicity_label, column_prefix) {
  data %>%
    filter(ET.Ethnic.Group.Category == ethnicity_label) %>%
    group_by(Year) %>%
    summarise(
      !!paste0("total_", column_prefix, "_students") := n(),
      !!paste0(column_prefix, "_completed_bioe_195") := sum(completed_bioe_195 == "Yes", na.rm = TRUE),
      ethnicity = ethnicity_label,
      .groups = "drop"
    ) %>%
    filter(!is.na(Year))
}

#Make abbreviations for different groups and group 
aab_completion <- process_ethnicity_data(bioe_students, "African-American/Black", "aab")
aian_completion <- process_ethnicity_data(bioe_students, "American Indian/Alaska Native", "aian")
asian_completion <- process_ethnicity_data(bioe_students, "Asian", "asian")
hisp_completion <- process_ethnicity_data(bioe_students, "Hispanic/Latino", "hisp")
hwi_completion <- process_ethnicity_data(bioe_students, "Native Hawaiian/Oth Pac Island", "hwi")
oth_completion <- process_ethnicity_data(bioe_students, "Other", "oth")
un_completion <- process_ethnicity_data(bioe_students, "Unknown/Not Specified", "un")
white_completion <- process_ethnicity_data(bioe_students, "White/Caucasian", "white")

#Plot by year and ethnicity
plot_ethnicity <- function(data, y_col, ethnicity_name, color) {
  print(paste("Checking:", ethnicity_name, "Column:", y_col, "Rows:", nrow(data)))  # Debugging
  if (nrow(data) > 0) {  # Only plot if data is available
    list(
      geom_line(data = data, aes(x = factor(Year), y = !!sym(y_col), color = ethnicity_name, group = 1), size = 1.5),
      geom_point(data = data, aes(x = factor(Year), y = !!sym(y_col), color = ethnicity_name), size = 4)
    )
  } else {
    NULL
  }
}

ethnicity_plot <- ggplot() +
  plot_ethnicity(aab_completion, "aab_completed_bioe_195", "African-American/Black", "#e76f51") +
  plot_ethnicity(aian_completion, "aian_completed_bioe_195", "American Indian/Alaska Native", "#f4a261") +
  plot_ethnicity(asian_completion, "asian_completed_bioe_195", "Asian", "#e9c46a") +
  plot_ethnicity(hisp_completion, "hisp_completed_bioe_195", "Hispanic/Latino", "#a65211") +
  plot_ethnicity(hwi_completion, "hwi_completed_bioe_195", "Native Hawaiian/Oth Pac Island", "#81400e") +
  plot_ethnicity(oth_completion, "oth_completed_bioe_195", "Other", "#2a9d8f") +
  plot_ethnicity(un_completion, "un_completed_bioe_195", "Unknown/Not Specified", "#76c893") +
  plot_ethnicity(white_completion, "white_completed_bioe_195", "White/Caucasian", "#264653") +  
  
  labs(
    x = "Year",
    y = "Number of Students Completing BIOE 195"
  ) +
  scale_color_manual(values = c(
    "African-American/Black" = "#e76f51",
    "American Indian/Alaska Native" = "#f4a261",
    "Asian" = "#e9c46a",
    "Hispanic/Latino" = "#a65211",
    "Native Hawaiian/Oth Pac Island" = "#81400e",
    "Other" = "#2a9d8f",
    "Unknown/Not Specified" = "#76c893",
    "White/Caucasian" = "#264653"
  )) +  
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    axis.title.y = element_blank()
    )

###Transfer x Completion
process_transfer_data <- function(data, transfer, column_prefix) {
  data %>%
    filter(UAD.Admit.Type.Descr == transfer) %>%
    group_by(Year) %>%
    summarise(
      !!paste0("total_", column_prefix, "_students") := n(),
      !!paste0(column_prefix, "_completed_bioe_195") := sum(completed_bioe_195 == "Yes", na.rm = TRUE),
      transfer_type = transfer, 
      .groups = "drop"
    ) %>%
    filter(!is.na(Year))
}

# Apply function for each transfer type and add abbreviations 
fresh_completion <- process_transfer_data(bioe_students, "First-Time Freshman", "fresh")
junior_t_completion <- process_transfer_data(bioe_students, "Junior Transfer", "junior_t")
senior_t_completion <- process_transfer_data(bioe_students, "Senior Transfer", "senior_t")
soph_t_completion <- process_transfer_data(bioe_students, "Sophomore Transfer", "soph_t")

# Plot
transfer_plot <- ggplot() +
  geom_line(data = fresh_completion, aes(x = factor(Year), y = fresh_completed_bioe_195, color = "First-Time Freshman", group = 1), size = 1.5) +
  geom_point(data = fresh_completion, aes(x = factor(Year), y = fresh_completed_bioe_195, color = "First-Time Freshman"), size = 4) +
  
  geom_line(data = junior_t_completion, aes(x = factor(Year), y = junior_t_completed_bioe_195, color = "Junior Transfer", group = 1), size = 1.5) +
  geom_point(data = junior_t_completion, aes(x = factor(Year), y = junior_t_completed_bioe_195, color = "Junior Transfer"), size = 4) +
  
  geom_line(data = senior_t_completion, aes(x = factor(Year), y = senior_t_completed_bioe_195, color = "Senior Transfer", group = 1), size = 1.5) +
  geom_point(data = senior_t_completion, aes(x = factor(Year), y = senior_t_completed_bioe_195, color = "Senior Transfer"), size = 4) +
  
  geom_line(data = soph_t_completion, aes(x = factor(Year), y = soph_t_completed_bioe_195, color = "Sophomore Transfer", group = 1), size = 1.5) +
  geom_point(data = soph_t_completion, aes(x = factor(Year), y = soph_t_completed_bioe_195, color = "Sophomore Transfer"), size = 4) +
  
  labs(
    x = "Year",
    y = "Number of Students Completing BIOE 195"
  ) +
  scale_color_manual(values = c("First-Time Freshman" = "#99e2b4", 
                                "Junior Transfer" = "#678d58", 
                                "Senior Transfer" = "#a6c48a", 
                                "Sophomore Transfer" = "#10451d")) +  
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    legend.title = element_blank()
    )

#Use patchwork to group plots together
final_demo_time <- (eop_plot | first_gen_plot) / (international_stat_plot | gender_plot) / (transfer_plot | ethnicity_plot)

# Display the final combined plot
print(final_demo_time)
