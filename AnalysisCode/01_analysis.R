# 01_analysis.R
# Senior thesis / field course analysis script
#
# Purpose:
#   Runs setup, data preparation, summary objects, and statistical models for the manuscript 
#   entitled "Pathways and predictors of independent research at a higher education institution" 
#   These ggplot objects are retained for downstream figure export in 02_figures_and_supp_tables.R.
#   Manuscript-quality ggsave() calls are kept in the figure/table script.

#
# Expected project structure:
#   data/bioe_students.csv
#   data/fieldcourse_info_aug26.csv   # or an existing object named field_course_info
#   outputs/figures/
#   outputs/tables/

## =========================
## HOUSEKEEPING
## =========================
# Load required packages for data cleaning, modeling, visualization, and summaries
# Startup messages are suppressed for cleaner output
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(here)
  library(janitor)
  library(broom)
  library(patchwork)
  library(ggthemes)
  library(scales)
  library(forcats)
  library(stringr)
  library(MuMIn)
  library(DHARMa)
  library(brglm)
  library(nnet)
  library(sandwich)
  library(lmtest)
  library(emmeans)
  library(gt)
  library(pwr)
})

# Set a consistent base plotting theme for all figures
theme_set(theme_minimal())

# Create output folders used by the analysis and manuscript outputs.
dir.create(here::here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "objects"), recursive = TRUE, showWarnings = FALSE)


## =========================
## CONSTANTS & PALETTES
## =========================
# Define the focal majors included in all downstream analyses.
majors_keep <- c(
  "Ecology and Evolution BS","Marine Biology BS","Biology BA",
  "Plant Sciences BS","Environmental Science BS"
)

# Define the set of field courses used to quantify field-based learning experiences.
# BIOE 195 is excluded from this list because it represents the thesis course.
field_courses_core <- c(
  "BIOE 82","BIOE 85","BIOE 128L","BIOE 141L","BIOE 151A",
  "BIOE 153A","BIOE 155L","BIOE 157A","BIOE 159A","BIOE 161L",
  "BIOE 150L","ENVS 107A","ENVS 104A"
)

# Define all courses for which per-row completion flags are created.
all_courses <- c(
  "BIOE 20F","BIOE 82","BIOE 85","BIOE 128L","BIOE 141L","BIOE 155L","BIOE 158L",
  "BIOE 145L","BIOE 157A","BIOE 161L","BIOE 159A","BIOE 150L","BIOE 151A",
  "ENVS 107A","BIOE 153A","BIOE 195","ENVS 104A"
)

# Establish a consistent color palette for key demographic variables
color_palette <- c(
  # EOP
  "Y"="#231942","N"="#e0b1cb",
  # Admit Type
  "First-Time Freshman"="#99e2b4","Junior Transfer"="#678d58",
  "Senior Transfer"="#a6c48a","Sophomore Transfer"="#10451d",
  # First Gen
  "First Generation"="#184e77","Not First Generation"="#52b69a",
  # Status
  "Domestic"="#d76a03","International"="#fabc3c",
  # Gender
  "F"="#bbd0ff","M"="#58148e","X"="#c18cf9","U"="#ffc4ff",
  # Ethnicity
  "African-American/Black"="#e76f51","American Indian/Alaska Native"="#f4a261",
  "Asian"="#e9c46a","Hispanic/Latino"="#a65211",
  "Native Hawaiian/Oth Pac Island"="#81400e","Other"="#2a9d8f",
  "Unknown/Not Specified"="#76c893","White/Caucasian"="#264653"
)

# Color palettes for GPA categories, field intensities, and thesis completion
gpa_palette   <- c("0"="#0a9396","1"="#94d2bd")
field_palette <- c("#540b0e","#bb3e03","#ca6702","#ee9b00","#fff3b0")
thesis_yes_no_palette <- c("Yes"="#0a9396","No"="#94d2bd")

## =========================
## HELPER FUNCTIONS
## =========================
# Convert UCSC term codes to calendar years.
# Handles both 4-digit (legacy) and 5-digit formats.
get_year <- function(tc) {
  s <- as.character(tc)
  n <- nchar(s)
  y4 <- suppressWarnings(as.integer(substr(s, 1, 4)))
  y3 <- suppressWarnings(as.integer(substr(s, 1, 3))) + 1800L
  ifelse(n == 5L, y4, ifelse(n == 4L, y3, NA_integer_))
}

# Convert the final digit of a term code into a quarter index (Fall=0, Winter=1, Spring=2, Summer=3).
# Enables construction of linear time indices.
get_quarter <- function(tc) {
  s <- as.character(tc)
  q <- substr(s, nchar(s), nchar(s))
  dplyr::case_when(
    q == "8" ~ 0L,
    q == "0" ~ 1L,
    q == "2" ~ 2L,
    q == "4" ~ 3L,
    TRUE ~ NA_integer_
  )
}

# Build a linear term index defined by year * 4 + quarter.
# Used for timing and progression analyses.
get_term_index <- function(tc) {
  y <- get_year(tc)
  qi <- get_quarter(tc)
  ifelse(is.na(y) | is.na(qi), NA_integer_, y * 4L + qi)
}

# Percent labels rounded to one decimal place (used in plots).
percent1 <- function(x) scales::percent(x, accuracy = 1)

## ===========================================
## LOAD IN FINAL DATASET FROM DATA PROCESSING
## ===========================================

bioe_students <- readr::read_csv(here::here("data", "bioe_students.csv"), show_col_types = FALSE)

# Field course metadata used for course intensity, field hours, units, and course attributes.
# If this object was already created upstream, it will be kept. Otherwise, the script reads it from data/.
if (!exists("field_course_info")) {
  field_course_info <- readr::read_csv(
    here::here("data", "fieldcourse_info_aug26.csv"),
    show_col_types = FALSE
  )
}

## =========================
## BASIC DISTRIBUTIONS
## =========================
course_names <- tribble(
  ~Course,     ~CourseLabel,
  "BIOE 20F",  "Field Bio. in Practice",
  "BIOE 82",   "Intro Field Research",
  "BIOE 85",   "Nat. History NRS",
  "BIOE 128L", "Large Marine Vert.",
  "BIOE 141L", "Behavioral Eco.",
  "BIOE 155L", "Freshwater Eco. Lab",
  "BIOE 158L", "Marine Eco. Lab",
  "BIOE 145L", "Plant Eco. Methods",
  "BIOE 157A", "Ichthyology Qtr",
  "BIOE 161L", "Kelp Forest Eco.",
  "BIOE 159A", "Marine Eco. Qtr",
  "BIOE 150L", "Eco. Field Methods",
  "BIOE 151A", "Eco. & Conservation",
  "ENVS 107A","Natural History Qtr",
  "BIOE 153A","Arctic Eco.",
  "ENVS 104A","Environ. Field Methods",
  "BIOE 195", "Senior Thesis"
)

## FIELD COURSE LOOKUP

field_lu <- field_course_info %>%
  transmute(
    SE.Subject.Catalog.Nbr = str_squish(as.character(SE.Subject.Catalog.Nbr)),
    TotalFieldHours        = suppressWarnings(as.numeric(Total.field.hours)),
    Independent.projects   = str_squish(as.character(Independent.projects)),
    TotalUnits             = suppressWarnings(as.numeric(Units)),
    StudentsinClass        = suppressWarnings(as.numeric(Students.per.Quarter)),
    Intensity              = as.character(Intensity)
  )

# Helper: base code without a single trailing A/B/C/D (keeps "L")
base_code <- function(x) sub("([0-9]{2,3})[A-D]$", "\\1", x)

# This creates a dataset with:
#   - each course (BIOE XX / ENVS XX)
#   - # of distinct students taking it
#   - readable labels
#   - matching intensity levels
bioe_courses <- bioe_students %>%
  
  # Restrict to courses in the field-course list
  filter(SE.Subject.Catalog.Nbr %in% all_courses) %>%
  
  # Count unique (Student, Course) combinations
  distinct(CodedID, SE.Subject.Catalog.Nbr) %>%
  
  # Make standardized fields
  transmute(
    Course     = str_squish(as.character(SE.Subject.Catalog.Nbr)),
    Course_base = base_code(Course)  # remove trailing A/B/C/D
  ) %>%
  
  # Count number of distinct students per course
  count(Course, Course_base, name = "Students") %>%
  
  # Join readable labels (BIOE 128L → “Field Ecology” etc)
  left_join(course_names, by = "Course") %>%
  mutate(CourseLabel = dplyr::coalesce(CourseLabel, Course)) %>%
  
  # Attach intensity levels
  left_join(
    field_lu %>% transmute(Course = SE.Subject.Catalog.Nbr, Intensity),
    by = "Course"
  ) %>%
  mutate(
    Intensity = dplyr::coalesce(
      Intensity,
      field_lu$Intensity[match(Course_base, field_lu$SE.Subject.Catalog.Nbr)]
    ),
    
    # Make BIOE 195 appear as "Research"
    Intensity = dplyr::case_when(
      Course == "BIOE 195" ~ "Research",
      TRUE ~ as.character(Intensity)
    ),
    
    Intensity = factor(
      Intensity,
      levels = c(
        "Introductory",
        "Low intensity",
        "Medium intensity",
        "Intensive",
        "Research"
      )
    )
  )

# quick sanity (optional):
# stopifnot(!any(is.na(bioe_courses$CourseLabel)))

#Plot field courses by intensity 
field_course_count <- ggplot(
  bioe_courses %>% mutate(CourseLabel = reorder(CourseLabel, Students)),
  aes(x = CourseLabel, y = Students, fill = Intensity)
) +
  geom_col(color = "grey20") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Introductory"     = "#ffe0e9",
    "Low intensity"    = "#ff7aa2",
    "Medium intensity" = "#cc718d",
    "Intensive"        = "#602437",
    "Research"         = "#0a9396"
  )) +
  labs(x = "Course", y = "Number of Students", fill = "Course Intensity") +
  theme_minimal() +
  theme(
    axis.text  = element_text(size = 14),
    axis.title = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 14)
  ); field_course_count

#ggsave(
#  filename = "./drafts/Figures/number_of_field_courses.png",
#  plot     = field_course_count,
#  width    = 8, height = 10, dpi = 600,
#  bg = 'white'
#) # Save figure for final manuscript 


## =========================
## FINAL-TERM STATUS + THESIS COMPLETION
## =========================
# Identify each student's most recent enrolled term and derive the corresponding academic year.
# Academic year is defined as the calendar year of the term unless the term is Fall (quarter == 8),
# where the academic year rolls forward to the subsequent calendar year.
latest_term <- bioe_students %>%
  group_by(CodedID) %>%
  filter(TA.Term.Code == max(TA.Term.Code, na.rm = TRUE)) %>%   # retain latest term per student
  ungroup() %>%
  mutate(
    year_raw      = as.numeric(substr(TA.Term.Code, 1, 3)) + 1800,  # convert term prefix to year
    quarter       = as.numeric(substr(TA.Term.Code, 4, 4)),         # final digit encodes quarter
    academic_year = if_else(quarter == 8, year_raw + 1, year_raw)   # Fall contributes to next academic year
  ) %>%
  distinct(CodedID, .keep_all = TRUE)  # enforce one row per student

# Create a table of all students who successfully completed BIOE 195 at least once.
thesis_yes_ids <- bioe_students %>%
  filter(completed_BIOE_195 == "Yes") %>%
  distinct(CodedID) %>%
  mutate(completed_thesis = "Yes")

# Attach thesis completion status to the final-term data.
# Students with no record of BIOE 195 completion are labeled as "No".
students_with_status <- latest_term %>%
  left_join(thesis_yes_ids, by = "CodedID") %>%
  mutate(
    completed_thesis = if_else(is.na(completed_thesis), "No", completed_thesis),
    completed_thesis = factor(completed_thesis, levels = c("No", "Yes"))
  ) %>%
  distinct(CodedID, .keep_all = TRUE)  # ensure one row per student

# Compute yearly counts of thesis vs. non-thesis students and convert to proportions.
yearly_counts <- students_with_status %>%
  count(academic_year, completed_thesis, name = "n") %>%
  group_by(academic_year) %>%
  mutate(
    total = sum(n),     # total students in that year
    prop  = n / total   # proportion completing/not completing a thesis
  ) %>%
  ungroup() %>%
  filter(!(academic_year %in% c(2010, 2011, 2025))) # remove the final partial academic year

# Extract total number of students per academic year for annotation.
year_totals <- yearly_counts %>%
  distinct(academic_year, total, completed_thesis)

# Visualize the proportion of students completing a thesis over time,
# with total counts annotated above each bar group.
p_basic_dist <- ggplot(
  yearly_counts,
  aes(
    x    = factor(academic_year),
    y    = prop,
    fill = completed_thesis
  )
) +
  geom_col(color = NA) +
  geom_text(
    data = year_totals,
    aes(x = factor(academic_year), y = 1, label = total),
    vjust = -0.6,
    size  = 5,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_fill_manual(
    values = thesis_yes_no_palette,
    labels = c("Yes" = "Thesis Completed", "No" = "No Thesis")
  ) +
  labs(
    x    = "Academic Year",
    y    = "Percentage of Students",
    fill = "Completed Thesis"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "right",
    plot.margin     = margin(t = 10, r = 5, b = 5, l = 5),
    axis.text       = element_text(size = 16),
    axis.title      = element_text(size = 22)
  ); p_basic_dist

#ggsave(
#  filename = "./drafts/Figures/p_basic_dist.png",
#  plot     = p_basic_dist,
#  width    = 12, height = 6, dpi = 600,
#  bg = 'white'
#) #Save figure for manuscript 

## =========================
## DROPPED VS. ENROLLED IN BIOE 195
## =========================

# Build a table of all BIOE 195 enrollment attempts and extract enrollment status.
bioe195_attempts <- bioe_students %>%
  filter(SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  transmute(
    CodedID,
    TA.Term.Code = as.character(TA.Term.Code),
    status = toupper(trimws(as.character(`SE.Student.Enrollment.Status.Code`)))
  ) %>%
  distinct(CodedID, TA.Term.Code, status)   # remove duplicate records per student-term


# Derive academic-year-of-attempt from term code and construct attempt-level indicators.
bioe195_attempts <- bioe195_attempts %>%
  mutate(
    year_raw = as.numeric(substr(TA.Term.Code, 1, 3)) + 1800,
    quarter  = as.numeric(substr(TA.Term.Code, 4, 4)),
    acad_year_attempt = if_else(quarter == 8, year_raw + 1, year_raw),
    dropped  = status == "D",
    enrolled = status == "E"
  )

# Summarize BIOE 195 participation at the student level:
# - ever_took: ever enrolled or dropped
# - ever_dropped: ever dropped
# - ever_enrolled: ever enrolled (with or without eventual drop)
bioe195_by_student <- bioe195_attempts %>%
  group_by(CodedID) %>%
  summarise(
    ever_took     = any(enrolled | dropped, na.rm = TRUE),
    ever_dropped  = any(dropped, na.rm = TRUE),
    ever_enrolled = any(enrolled, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  filter(ever_took)  # restrict to students who interacted with BIOE 195


# Compute the overall proportion of students who dropped BIOE 195 among those who ever attempted it.
overall_195 <- bioe195_by_student %>%
  summarise(
    n_took    = n(),
    n_dropped = sum(ever_dropped),
    prop_dropped_among_takers = n_dropped / n_took
  ); print(overall_195)


# Calculate drop rates by academic year of the BIOE 195 attempt.
# Students may appear in multiple years if they attempted the course more than once.
by_year_attempt <- bioe195_attempts %>%
  filter(enrolled | dropped) %>%
  distinct(CodedID, acad_year_attempt, status) %>%
  group_by(acad_year_attempt) %>%
  summarise(
    n_took      = n_distinct(CodedID),
    n_dropped   = n_distinct(CodedID[status == "D"]),
    prop_dropped = if_else(n_took > 0, n_dropped / n_took, NA_real_),
    .groups     = "drop"
  ) %>%
  arrange(acad_year_attempt)

## =========================
## PREPARE DEMOGRAPHIC FRAME FOR GAP ANALYSIS
## =========================
# Demographic variables to include in gap calculations
dems <- c("Gender", "Race/Ethnicity", "Status", "Admit Type", "First Generation", "EOP")

# Levels to exclude from denominators (unknown, other, or low-information categories)
levels_to_drop <- c("Unknown", "-", "Unknown/Other", "Senior Transfer", "Other", "U")

# Identify all students who successfully completed BIOE 195 at least once
# (EEB subset used for the gap analysis).
thesis_yes_ids_eeb <- bioe_students %>%
  filter(completed_BIOE_195 == "Yes") %>%
  distinct(CodedID) %>%
  mutate(completed_thesis = "Yes")

# Extract each student's latest enrolled term and derive academic year for the EEB subset.
latest_term_eeb <- bioe_students %>%
  group_by(CodedID) %>%
  filter(TA.Term.Code == max(TA.Term.Code, na.rm = TRUE)) %>%  # retain final observed term per student
  ungroup() %>%
  mutate(
    year_raw      = as.numeric(substr(TA.Term.Code, 1, 3)) + 1800,  # convert term prefix to year
    quarter       = as.numeric(substr(TA.Term.Code, 4, 4)),
    academic_year = if_else(quarter == 8, year_raw + 1, year_raw)   # Fall contributes to subsequent academic year
  )

# Attach thesis completion status to the latest-term records for the EEB subset.
students_with_status_eeb <- latest_term_eeb %>%
  left_join(thesis_yes_ids_eeb, by = "CodedID") %>%
  mutate(
    completed_thesis = if_else(is.na(completed_thesis), "No", completed_thesis),
    completed_thesis = factor(completed_thesis, levels = c("No", "Yes"))
  )

# Build a clean, student-level frame with recoded demographic columns and final thesis status.
bioe_students_clean <- latest_term_eeb %>%
  left_join(
    students_with_status_eeb %>% dplyr::select(CodedID, completed_thesis),
    by = "CodedID"
  ) %>%
  rename(
    "Gender"          = "PD.Gender...Current",
    "Race/Ethnicity"  = "ET.Ethnic.Group.Category",
    "Status"          = "PD.Domestic.International...Current",
    "Admit Type"      = "UAD.Admit.Type.Descr",
    "First Generation"= "UAD.First.Generation",
    "EOP"             = "SGC.Is.EOP"
  ) %>%
  dplyr::select(CodedID, academic_year, completed_thesis, all_of(dems)) %>%
  mutate(across(all_of(dems), ~ forcats::as_factor(as.character(.x)))) %>%  # standardize factor coding
  droplevels() %>%
  distinct(CodedID, .keep_all = TRUE)  # ensure one row per student

## =========================
## COMPUTE PARTICIPATION GAPS
## =========================
# Function to compute representation vs. thesis participation for each demographic dimension.
# If by_year = TRUE, gaps are computed separately by academic year; otherwise, years are pooled.
compute_gap <- function(df, by_year = FALSE) {
  # Attach a Year column when needed; otherwise set to NA for pooled analysis.
  res <- if (by_year) {
    df %>% rename(Year = academic_year)
  } else {
    df %>% mutate(Year = NA_integer_)
  }
  
  purrr::map_dfr(dems, function(var) {
    res %>%
      # Drop unknown/other only for the current demographic variable
      mutate(value_raw = trimws(as.character(.data[[var]]))) %>%
      filter(!(value_raw %in% levels_to_drop)) %>%
      group_by(Year, value = forcats::as_factor(value_raw), completed_thesis) %>%
      summarise(n = n(), .groups = "drop") %>%
      # Ensure that both thesis outcomes ("No", "Yes") are present for every (Year, value) combination
      tidyr::complete(
        Year, value,
        completed_thesis = factor(c("No", "Yes"), levels = c("No", "Yes")),
        fill = list(n = 0L)
      ) %>%
      # Reshape to wide format: counts of ThesisYes and ThesisNo per level
      tidyr::pivot_wider(
        names_from  = completed_thesis,
        values_from = n,
        names_prefix = "Thesis"
      ) %>%
      mutate(
        ThesisYes = tidyr::replace_na(ThesisYes, 0L),
        ThesisNo  = tidyr::replace_na(ThesisNo,  0L),
        Total     = ThesisYes + ThesisNo
      ) %>%
      group_by(Year) %>%
      mutate(
        # DemRate: demographic representation in the total student population
        DemRate    = Total / sum(Total),
        # ThesisRate: share of all theses completed by that demographic level
        tot_yes    = sum(ThesisYes),
        ThesisRate = if_else(tot_yes > 0, ThesisYes / tot_yes, NA_real_),
        # Participation gap: difference between thesis representation and underlying representation
        ThesisGap  = ThesisRate - DemRate
      ) %>%
      ungroup() %>%
      dplyr::select(-tot_yes) %>%
      mutate(Demographic = var)
  }) %>%
    rename(Value = value)
}

# Compute overall (pooled across years) participation gaps and year-specific gaps.
agg_all    <- compute_gap(bioe_students_clean, by_year = FALSE)
agg_all_yr <- compute_gap(bioe_students_clean, by_year = TRUE)


## =========================
## TESTS OF GAPS BY DEMOGRAPHIC
## =========================

# Wrapper to test whether thesis rates differ across levels within each demographic,
# using a chi-squared test on the ThesisYes/Total counts and reporting Cramér's V.
test_gaps_overall <- function(agg_all) {
  agg_all %>%
    group_by(Demographic) %>%
    summarise(
      # Extract chi-square statistic from a proportion test on per-level counts.
      chisq = {
        x <- ThesisYes
        n <- Total
        if (length(x) >= 2 && all(is.finite(x)) && all(is.finite(n)) && all(n > 0)) {
          suppressWarnings(prop.test(x = x, n = n)$statistic)
        } else NA_real_
      },
      # Degrees of freedom = number of levels - 1 for the demographic factor.
      df = {
        k <- n()  # number of demographic levels
        if (k >= 2) k - 1 else NA_integer_
      },
      # Associated p-value from the same proportion test.
      p_value = {
        x <- ThesisYes
        n <- Total
        if (length(x) >= 2 && all(is.finite(x)) && all(is.finite(n)) && all(n > 0)) {
          suppressWarnings(prop.test(x = x, n = n)$p.value)
        } else NA_real_
      },
      # N: total number of students contributing to that demographic.
      N        = sum(Total, na.rm = TRUE),
      k_levels = n(),
      .groups  = "drop_last"
    ) %>%
    mutate(
      # Cramér's V summarizing effect size for a 2 × k contingency table.
      # With 2 outcome categories (Thesis Yes/No), min(r-1, c-1) = 1, so denominator reduces to N.
      cramer_v = ifelse(
        is.finite(chisq) & is.finite(N) & N > 0,
        sqrt(as.numeric(chisq) / (N * 1)),
        NA_real_
      )
    ) %>%
    ungroup() %>%
    mutate(p_adj = p.adjust(p_value, method = "BH")) %>%  # FDR-adjusted p-values across demographics
    arrange(p_value)
}

overall_gap_tests <- test_gaps_overall(agg_all); overall_gap_tests


# Compute the mean absolute participation gap per demographic and year,
# providing a scalar measure of inequality over time.
gap_trend <- agg_all_yr %>%
  mutate(abs_gap = abs(ThesisGap)) %>%
  group_by(Demographic, Year) %>%
  summarise(
    mean_abs_gap = mean(abs_gap, na.rm = TRUE),
    .groups      = "drop"
  )

# Fit simple linear models (mean_abs_gap ~ Year) to assess whether participation
# gaps are widening or narrowing over time for each demographic factor.
gap_trend_models <- gap_trend %>%
  group_by(Demographic) %>%
  summarise(
    slope   = coef(lm(mean_abs_gap ~ Year))[2],
    p_value = summary(lm(mean_abs_gap ~ Year))$coefficients[2, 4],
    r2      = summary(lm(mean_abs_gap ~ Year))$r.squared
  ); gap_trend_models

## ========================================================
## A) LOGISTIC MODEL FOR FIELD COURSE IMPACTS AND PLOTTING
## ========================================================
model_courses <- setdiff(all_courses, c("BIOE 195", "BIOE 158L"))

# Build student-level “ever took” flags for each course
mn_flag_cols <- paste0("completed_", gsub(" ", "_", model_courses))

student_flags <- bioe_students %>%
  group_by(CodedID) %>%
  summarise(
    # Outcome: ever completed BIOE 195
    completed_BIOE_195 = any(completed_BIOE_195 == "Yes", na.rm = TRUE),
    
    # Predictors: for each course, 1 if student ever took it, 0 otherwise
    across(all_of(mn_flag_cols), ~ any(. == "Yes", na.rm = TRUE)),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Convert predictors to 0/1
    across(all_of(mn_flag_cols), as.integer),
    
    # Convert outcome to factor for multinom
    completed_BIOE_195 = factor(
      as.integer(completed_BIOE_195),
      levels = c(0, 1)
    )
  ) %>%
  distinct(CodedID, .keep_all = TRUE)

# Use all course flags as predictors
course_cols <- mn_flag_cols

# Check that all 16 predictor courses are included
length(course_cols)
course_cols

# Logistic model: P(thesis) ~ took each course
model_formula <- reformulate(
  termlabels = course_cols,
  response = "completed_BIOE_195"
)

multi_mod_all_courses <- nnet::multinom(
  model_formula,
  data = student_flags,
  trace = FALSE
)

summary(multi_mod_all_courses)

all_course_results <- broom::tidy(multi_mod_all_courses) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    SE = std.error,
    z = statistic,
    p_value = p.value,
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  dplyr::select(
    Term = term,
    Estimate = estimate,
    SE,
    z,
    p_value,
    sig
  ); all_course_results

#write.csv(
#  all_course_results,
#  here::here("outputs", "tables", "all_course_multinom_results.csv"),
#  row.names = FALSE
#)


## B) AVERAGE PREDICTED PROBABILITIES (TOGGLE APPROACH)
# For each course flag, compute the average predicted probability of thesis completion
# if (a) no one took the course (flag = 0) vs (b) everyone took it (flag = 1).
# This yields p0, p1 and a ∆ in percentage points (d_pp) and a risk ratio (rr).
compute_toggle_probs <- function(model, data, cols, outcome = "1") {
  
  # Helper: extract predicted probability for the thesis-complete outcome.
  get_prob1 <- function(nd) {
    p <- predict(model, newdata = nd, type = "probs")
    if (is.matrix(p) && !is.null(colnames(p)) && outcome %in% colnames(p)) {
      as.numeric(p[, outcome])
    } else if (is.matrix(p)) {
      as.numeric(p[, ncol(p)])  # last column if unnamed
    } else {
      as.numeric(p)
    }
  }
  
  purrr::map_dfr(cols, function(col) {
    nd0 <- dplyr::mutate(data, !!col := 0L)
    nd1 <- dplyr::mutate(data, !!col := 1L)
    tibble::tibble(
      course_flag = col,
      p0 = mean(get_prob1(nd0), na.rm = TRUE),
      p1 = mean(get_prob1(nd1), na.rm = TRUE)
    )
  })
}

ame_tbl <- compute_toggle_probs(multi_mod_all_courses, student_flags, course_cols) %>%
  dplyr::mutate(
    Course = gsub("_", " ", sub("^completed_", "", course_flag)),
    d_pp   = (p1 - p0) * 100,                     # ∆ in percentage points
    rr     = ifelse(p0 > 0, p1 / p0, NA_real_)    # risk ratio
  )

## C) ODDS RATIOS AND SIGNIFICANT COURSES
# Extract log-odds, convert to odds ratios, and collect Wald CIs and p-values.
or_tbl <- broom::tidy(multi_mod_all_courses, conf.int = TRUE) %>%
  dplyr::filter(term %in% course_cols) %>%
  dplyr::transmute(
    course_flag = term,
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high),
    p.value
  )

# Combine AME-style probabilities and ORs, then keep only statistically significant courses.
results <- ame_tbl %>%
  dplyr::left_join(or_tbl, by = "course_flag") %>%
  dplyr::select(Course, p0, p1, d_pp, rr, OR, OR_low, OR_high, p.value) %>%
  dplyr::arrange(dplyr::desc(d_pp)) %>%
  dplyr::filter(!is.na(p.value), p.value < 0.05)

results_sig <- results

## =========================
## DEGREE-TIMING & FIELD-COURSES
## =========================
# Build per-row timing features relative to each student's admit→grad window.
# - admit_term, course_term, grad_term: numeric indices for each key term
# - denom: # of quarter-steps from admit to grad (min 1 to avoid /0)
# - raw_pct: where a course term falls between admit and grad
# - course_percentage_degree: clamp raw_pct to [0,1] for clean summaries
# - year: calendar year of admission (used later in student-level summaries/models)
time_completed_summary <- bioe_students %>%
  mutate(
    admit_term  = get_term_index(UAD.Admit.Term.Code),      # numeric index for admit term
    course_term = get_term_index(TA.Term.Code),             # numeric index for course term
    grad_term   = get_term_index(DG.Completion.Term.Code),  # numeric index for grad term
    denom = if_else(
      is.na(admit_term) | is.na(grad_term),
      NA_integer_,
      pmax(1L, grad_term - admit_term)
    ),
    raw_pct = (course_term - admit_term) / denom,           # raw fraction of degree completed
    course_percentage_degree = pmin(pmax(raw_pct, 0), 1),   # clamp to [0,1]
    year = get_year(UAD.Admit.Term.Code)                    # admit year (calendar year)
  )

# Timing summary (median timing per course + IQR + N)
course_set <- bioe_courses %>% arrange(Students) %>% pull(Course)

avg_course_pct_cs <- time_completed_summary %>%
  filter(SE.Subject.Catalog.Nbr %in% course_set) %>%        # keep only courses in course_set
  group_by(SE.Subject.Catalog.Nbr) %>%
  summarise(
    median_pct = median(course_percentage_degree, na.rm = TRUE),              # median timing
    lq         = quantile(course_percentage_degree, 0.25, na.rm = TRUE),      # 25th percentile
    uq         = quantile(course_percentage_degree, 0.75, na.rm = TRUE),      # 75th percentile
    n          = n(),                                                         # number of enrollments
    Course     = first(SE.Subject.Catalog.Nbr),
    .groups    = "drop"
  ) %>%
  left_join(course_names, by = c("Course" = "Course")) %>%                    # add course labels
  left_join(field_lu %>% dplyr::select(SE.Subject.Catalog.Nbr, Intensity),
            by = c("Course" = "SE.Subject.Catalog.Nbr")) %>%
  mutate(
    # Tag BIOE 195 as "Research" intensity
    Intensity = case_when(
      Course == "BIOE 195" ~ "Research",
      TRUE ~ as.character(Intensity)
    ),
    Intensity = factor(
      Intensity,
      levels = c("Introductory", "Low intensity", "Medium intensity", "Intensive", "Research")
    ),
    CourseLabel = coalesce(CourseLabel, Course),            # fallback to code if no label
    CourseLabel = fct_reorder(CourseLabel, median_pct)      # order labels by median timing
  )

## =========================
## PRIOR-COURSE INDICATOR & GLMM
## =========================
# Goal: did students who completed 195 previously take ANY course with the same instructor?
# We match each 195 attempt to the earliest prior non-195 course with the same instructor.

# Student-level outcome: ever took BIOE 195 (0/1)
student_summary1 <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%
  group_by(CodedID) %>%
  summarise(completed_BIOE_195 = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 195")),
            .groups = "drop")

# BIOE 195 attempts with instructor + term
bioe195_inst <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  mutate(
    CodedID              = as.character(CodedID),
    bioe195_instructor   = str_squish(as.character(CI.Instructor.Name)),
    TA.Term.Code_bioe195 = suppressWarnings(as.integer(TA.Term.Code))
  ) %>%
  distinct(CodedID, bioe195_instructor, TA.Term.Code_bioe195)

# Prior (non-195) enrollments for those students
history_all <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr != "BIOE 195",
         CodedID %in% bioe195_inst$CodedID) %>%
  transmute(
    CodedID          = as.character(CodedID),
    prior_instructor = str_squish(as.character(CI.Instructor.Name)),
    TA.Term.Code_prior = suppressWarnings(as.integer(TA.Term.Code)),
    prior_course     = as.character(SE.Subject.Catalog.Nbr)
  ) %>%
  distinct()

# For each 195 attempt, keep the earliest prior course with the SAME instructor
prior_match_details <- history_all %>%
  inner_join(
    bioe195_inst,
    by = join_by(CodedID,
                 TA.Term.Code_prior < TA.Term.Code_bioe195,
                 prior_instructor  == bioe195_instructor)
  ) %>%
  group_by(CodedID, TA.Term.Code_bioe195) %>%
  slice_min(TA.Term.Code_prior, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(CodedID, prior_instructor, TA.Term.Code_prior, prior_course, TA.Term.Code_bioe195)

# Student-level: flag whether any prior same-instructor course exists
student_match_full <- student_summary1 %>%
  left_join(prior_match_details, by = "CodedID") %>%
  mutate(had_prior_course = !is.na(prior_instructor))

# Permutation test: are advisors “more likely than chance” to be prior instructors?
set.seed(42)

# Thesis roster + their actual BIOE 195 advisors
thesis_roster <- student_match_full %>%
  filter(completed_BIOE_195 == 1) %>%
  dplyr::select(CodedID) %>%
  left_join(bioe195_inst %>% transmute(CodedID, bioe195_instructor = str_squish(bioe195_instructor)),
            by = "CodedID") %>%
  distinct()

# For those students, collect all prior non-195 instructors (set)
course_filter <- NULL  # set to all_courses to restrict if desired
prior_sets <- time_completed_summary %>%
  mutate(
    CodedID    = as.character(CodedID),
    course     = as.character(SE.Subject.Catalog.Nbr),
    instructor = str_squish(as.character(CI.Instructor.Name))
  ) %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         CodedID %in% thesis_roster$CodedID,
         course != "BIOE 195",
         is.null(course_filter) | course %in% course_filter) %>%
  distinct(CodedID, instructor) %>%
  group_by(CodedID) %>%
  summarise(prior_instructors = list(unique(instructor)), .groups = "drop")

# Merge and compute observed match rate
perm_df <- thesis_roster %>%
  left_join(prior_sets, by = "CodedID") %>%
  mutate(prior_instructors = lapply(prior_instructors, \(x) if (is.null(x)) character(0) else x))

obs_prop <- mean(
  mapply(function(inst, set) inst %in% set, perm_df$bioe195_instructor, perm_df$prior_instructors),
  na.rm = TRUE
)

# Null via permutation (shuffle advisors across students)
n_perm <- 10000L
advisors <- perm_df$bioe195_instructor
perm_props <- replicate(n_perm, {
  shuffled <- sample(advisors, length(advisors), replace = FALSE)
  mean(mapply(`%in%`, shuffled, perm_df$prior_instructors))
})
p_val <- mean(perm_props >= obs_prop)

tibble::tibble(
  metric = c("Observed % with prior advisor", "Null mean % (random assignment)", "Permutation p-value (one-sided)"),
  value  = c(sprintf("%.1f%%", 100*obs_prop),
             sprintf("%.1f%%", 100*mean(perm_props)),
             format.pval(p_val, digits = 3, eps = 1e-4))
) %>% print(n = Inf)

## =========================
## STUDENT-SPECIFIC MODELS
## =========================
# Build student-level flags (ever took key field courses) AND
# timing for the *first* time they took each course (in % of degree).

student_summary <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%   # only enrolled course records
  group_by(CodedID) %>%
  summarise(
    # outcome: ever completed BIOE 195 (0/1)
    completed_BIOE_195  = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 195")),
    
    # ever-completed flags (selected field courses; 0/1)
    completed_BIOE_82   = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 82")),
    completed_BIOE_128L = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 128L")),
    completed_BIOE_141L = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 141L")),
    completed_BIOE_151A = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 151A")),
    completed_BIOE_155L = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 155L")),
    completed_BIOE_145L = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 145L")),
    
    # timing of first attempt for each course (min % of degree among that course)
    time_pct_BIOE_82   = ifelse(completed_BIOE_82,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 82"],   na.rm = TRUE), NA_real_),
    time_pct_BIOE_128L = ifelse(completed_BIOE_128L,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 128L"], na.rm = TRUE), NA_real_),
    time_pct_BIOE_141L = ifelse(completed_BIOE_141L,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 141L"], na.rm = TRUE), NA_real_),
    time_pct_BIOE_151A = ifelse(completed_BIOE_151A,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 151A"], na.rm = TRUE), NA_real_),
    time_pct_BIOE_155L = ifelse(completed_BIOE_155L,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 155L"], na.rm = TRUE), NA_real_),
    time_pct_BIOE_145L = ifelse(completed_BIOE_145L,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 145L"], na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# Latest available cumulative GPA per student (based on most recent term index)
gpa_latest <- time_completed_summary %>%
  transmute(
    CodedID = as.character(CodedID),
    term_i  = get_term_index(TA.Term.Code),                               # numeric term index
    gpa_i   = suppressWarnings(as.numeric(as.character(`TA.GPA...Cumulative`)))
  ) %>%
  filter(!is.na(CodedID), !is.na(term_i), !is.na(gpa_i)) %>%              # drop incomplete rows
  group_by(CodedID) %>%
  slice_max(term_i, n = 1, with_ties = FALSE) %>%                         # keep latest GPA per student
  ungroup() %>%
  dplyr::select(CodedID, gpa_raw = gpa_i)

# Admit year from admit term code (calendar year at admission)
admit_year_tbl <- bioe_students %>%
  transmute(
    CodedID = as.character(CodedID),
    year = get_year(UAD.Admit.Term.Code)  # helper that parses term code → calendar year
  ) %>%
  distinct(CodedID, .keep_all = TRUE)

# Field-course summary per student:
# - n_field_courses: distinct # of field courses taken
# - term_code: latest field-course term index
# - plus admit year and latest GPA joined in later
field_summary <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr %in% all_courses) %>%     # restrict to any field course
  mutate(
    CodedID = as.character(CodedID),
    term_i  = get_term_index(TA.Term.Code)
  ) %>%
  group_by(CodedID) %>%
  summarise(
    n_field_courses = n_distinct(SE.Subject.Catalog.Nbr),   # how many distinct field courses
    term_code       = if (all(is.na(term_i))) NA_integer_ else max(term_i, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(                                          # keep all students in time_completed_summary
    time_completed_summary %>% distinct(CodedID = as.character(CodedID)),
    by = "CodedID"
  ) %>%
  mutate(n_field_courses = replace_na(n_field_courses, 0L)) %>%  # non-takers → 0 field courses
  left_join(admit_year_tbl, by = "CodedID") %>%                  # add admit year
  left_join(gpa_latest,     by = "CodedID")                      # add latest cumulative GPA

# Demographics (use latest record per student by term)
demo_vars <- bioe_students %>%
  mutate(
    CodedID = as.character(CodedID),
    term    = suppressWarnings(as.integer(TA.Term.Code))
  ) %>%
  arrange(CodedID, desc(term)) %>%                   # sort so latest term is first per student
  group_by(CodedID) %>%
  slice(1) %>%                                       # keep most recent row for each student
  ungroup() %>%
  dplyr::select(
    CodedID,
    UAD.Admit.Type.Descr,
    PD.Gender...Current,
    SGC.Is.EOP,
    PD.Domestic.International...Current,
    ET.Ethnic.Group.Category,
    UAD.First.Generation,
    TotalFieldHours,
    Independent.projects,
    UAD.Applicant.GPA,
    Major = PLNA.Plan.Descr
  )

# Assemble master student-level frame
model_students <- student_summary %>%
  left_join(field_summary,      by = "CodedID") %>%  # add field-course counts & year/GPA
  left_join(student_match_full, by = "CodedID") %>%  # add prior-course indicator
  left_join(demo_vars,          by = "CodedID") %>%  # add demographics
  rename_with(~ sub("\\.y$", "", .), ends_with(".y")) %>%  # clear name suffixes from joins
  dplyr::select(-ends_with(".x")) %>%
  mutate(
    # average timing across any field courses taken (row mean across time_pct_* columns)
    avg_time_pct_field   = rowMeans(across(starts_with("time_pct_")), na.rm = TRUE),
    avg_time_pct_field_s = as.numeric(scale(avg_time_pct_field)),         # scaled timing
    completed_BIOE_195   = factor(completed_BIOE_195, levels = c(0, 1))   # 0/1 as factor
  ) %>%
  filter(!is.na(completed_BIOE_195)) %>%               # drop any rows w/o known thesis status
  distinct(CodedID, .keep_all = TRUE)

# Clean levels & scale GPA; set reference levels for categorical predictors
model_students2 <- model_students %>%
  mutate(
    UAD.Applicant.GPA = na_if(UAD.Applicant.GPA, 0)    # treat 0 as missing applicant GPA
  ) %>%
  mutate(
    GPA_scaled       = as.numeric(scale(gpa_raw)),      # scale latest cumulative GPA
    app_GPA_scaled   = as.numeric(scale(UAD.Applicant.GPA)),  # scale applicant GPA
    had_prior_course = factor(had_prior_course, levels = c(FALSE, TRUE)),
    total_field_hours   = suppressWarnings(as.numeric(TotalFieldHours)),
    independent.project = factor(Independent.projects, levels = c("Yes", "No")),
    Major = factor(Major),
    # Set reference levels
    UAD.Admit.Type.Descr     = fct_relevel(UAD.Admit.Type.Descr, "First-Time Freshman"),
    PD.Gender...Current      = fct_relevel(PD.Gender...Current,  "F"),
    SGC.Is.EOP               = fct_relevel(SGC.Is.EOP,           "N"),
    ET.Ethnic.Group.Category = fct_relevel(ET.Ethnic.Group.Category, "White/Caucasian"),
    UAD.First.Generation     = fct_relevel(UAD.First.Generation, "Not First Generation")
  ) %>%
  distinct(CodedID, .keep_all = TRUE) 

# Minimal modeling frame: keep only variables used in the logistic model
vars_used <- c(
  "completed_BIOE_195","n_field_courses","avg_time_pct_field_s",
  "UAD.Admit.Type.Descr","PD.Gender...Current","SGC.Is.EOP",
  "year","ET.Ethnic.Group.Category","UAD.First.Generation",
  "GPA_scaled","CodedID","total_field_hours", "app_GPA_scaled", "Major"
)

model_df <- model_students2 %>%
  dplyr::select(all_of(vars_used)) %>%
  mutate(completed_BIOE_195 = factor(completed_BIOE_195, levels = c(0, 1))) %>%
  droplevels() %>%
  filter(!is.na(UAD.Admit.Type.Descr)) %>%
  mutate(
    Status_simple = factor(
      ifelse(
        UAD.Admit.Type.Descr %in% c(
          "Junior Transfer",
          "Sophomore Transfer",
          "Senior Transfer"
        ),
        "Transfer",
        "First-Time Freshman"
      ),
      levels = c("First-Time Freshman", "Transfer")
    ),
    year_s = as.numeric(scale(year))
  ) %>%
  filter(
    # Race/Ethnicity
    !ET.Ethnic.Group.Category %in% c(
      " -",
      "Other",
      "Unknown/Not Specified"
    ),
    
    # First Generation
    !UAD.First.Generation %in% c(
      "Unknown/Other"
    ),
    
    # Gender
    !PD.Gender...Current %in% c(
      "U"
    )
  ) %>%
  droplevels()

model_df <- model_df %>%
  mutate(
    PEER = case_when(
      ET.Ethnic.Group.Category %in% c(
        "African-American/Black",
        "Hispanic/Latino",
        "American Indian/Alaska Native",
        "Native Hawaiian/Oth Pac Island"
      ) ~ "PEER",
      
      ET.Ethnic.Group.Category %in% c(
        "White/Caucasian",
        "Asian"
      ) ~ "Non-PEER",
      
      TRUE ~ NA_character_
    ),
    PEER = factor(PEER, levels = c("Non-PEER", "PEER"))
  )

# Logistic model on minimal frame:
# P(thesis completion) ~ field-course count + demographics + GPA
glm_student2_clean <- glm(
  completed_BIOE_195 ~ n_field_courses + Major + 
    Status_simple + PD.Gender...Current + SGC.Is.EOP +
    PEER + UAD.First.Generation + GPA_scaled + app_GPA_scaled,
  data = model_df,
  family = binomial("logit")
)
summary(glm_student2_clean)
r.squaredGLMM(glm_student2_clean)                     # marginal/conditional R2
cor.test(model_df$n_field_courses, model_df$avg_time_pct_field_s)  # correlation check
# robust_ct(glm_student2_clean,   model_df, fallback = "year") # ICC close to 0, do not compute robust transform


emmeans(glm_student2_clean, ~ Major, type = "response")

# Quick visuals: field-course count distribution by thesis completion
gpa_palette <- c("0" = "#0a9396", "1" = "#94d2bd")

# Quick visuals: scaled GPA by thesis completion
p_gpa <- ggplot(
  model_df,
  aes(x = factor(completed_BIOE_195),
      y = GPA_scaled,
      fill = factor(completed_BIOE_195))
) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "grey20") +
  geom_jitter(width = 0.22, size = 1.4, alpha = 0.3, color = "grey30") +
  scale_fill_manual(values = gpa_palette, labels = c("0 = No","1 = Yes")) +
  labs(x = "Senior Thesis Completion", y = "Scaled GPA", fill = "Thesis Completion") +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank()); p_gpa

# ORs from the logistic model
or1_clean <- broom::tidy(glm_student2_clean, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    term,
    OR     = exp(estimate),
    OR_low = exp(conf.low),
    OR_high= exp(conf.high)
  ) %>%
  mutate(term = case_when(
    term == "n_field_courses"                      ~ "Number of Field Courses",
    term == "avg_time_pct_field_s"                 ~ "Avg. Timing of Field Courses",
    term == "n_field_courses:avg_time_pct_field_s" ~ "Field Courses × Timing",
    term == "GPA_scaled"                           ~ "GPA (scaled)",
    TRUE ~ term
  ))
print(or1_clean)

# Collinearity check for predictors
car::vif(glm_student2_clean)


## =========================
## PER-COURSE TIMING GLM
## (Took + Timing per course, plus GPA)
## =========================

## 0) Course set to analyze
all_courses <- c("BIOE 82","BIOE 128L","BIOE 141L","BIOE 155L","BIOE 151A")

## 1) Student-level thesis outcome (response: ever completed BIOE 195)
# For each student, flag whether they ever completed BIOE 195 (0/1).
stu_outcome <- time_completed_summary %>%
  mutate(CodedID = as.character(CodedID)) %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%    #enrolled rows only
  group_by(CodedID) %>%
  summarise(
    completed_BIOE_195 = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 195")),
    .groups = "drop"
  )


## 2) BIOE 195 attempt info (for “prior same-instructor” checks)
# Extract BIOE 195 rows with instructor and term code, one row per student–instructor–term.
bioe195_inst <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  mutate(
    CodedID              = as.character(CodedID),
    bioe195_instructor   = stringr::str_squish(as.character(CI.Instructor.Name)),
    TA.Term.Code_bioe195 = suppressWarnings(as.integer(TA.Term.Code))
  ) %>%
  distinct(CodedID, bioe195_instructor, TA.Term.Code_bioe195)

## 3) Predictor-side enrollments
##    Keep ONLY courses in all_courses; EXCLUDE 195 rows
##    Add per-row % of degree completed (already computed upstream)
enroll_field <- time_completed_summary %>%
  mutate(
    CodedID    = as.character(CodedID),
    course     = as.character(SE.Subject.Catalog.Nbr),
    term       = suppressWarnings(as.integer(TA.Term.Code)),
    instructor = stringr::str_squish(as.character(CI.Instructor.Name))
  ) %>%
  filter(
    SE.Student.Enrollment.Status.Code == "E",
    course %in% all_courses,
    course != "BIOE 195" #build row-level table of field-course enrollments without BIOE 195
  ) %>%
  # keep the admission year that was already computed upstream in time_completed_summary
  distinct(CodedID, course, term, instructor, year, .keep_all = TRUE) %>% #keep one record per student x course x term x instructor
  dplyr::select(CodedID, course, term, instructor, course_percentage_degree, year) #keep the admit-term from time_completed_summary  

## 4) Course attributes (join)
# Attach course-level metadata (field hours, units, independent projects, intensity)
course_info <- field_course_info %>%
  transmute(
    course          = as.character(SE.Subject.Catalog.Nbr),
    TotalFieldHours = suppressWarnings(as.numeric(Total.field.hours)),
    TotalUnits      = suppressWarnings(as.numeric(Units)),
    indep_proj_any  = as.integer(Independent.projects %in% c("Yes","Y",1,TRUE)),
    Intensity       = as.character(Intensity)
  ) %>%
  filter(course %in% all_courses) %>%
  distinct(course, .keep_all = TRUE)

enroll_field <- enroll_field %>% left_join(course_info, by = "course") #join files together

## 5) Student-level summaries from non-195 enrollments
# Collapse row-level enrollments into a student-level field-course profile:
# - n_field_courses: distinct # of core field courses taken
# - avg_time_pct_field: mean timing (% degree) across all field courses
# - TotalFieldHours, TotalUnits: summed across field courses
# - Intensity: first intensity label (shared across these courses)
# - indep_proj_any: indicator if any course had an independent project
# - year: student's admit year (constant per student)
# - instructor_primary: instructor from earliest field course (by term)

student_field_profile <- enroll_field %>%
  group_by(CodedID) %>%
  summarise(
    n_field_courses    = n_distinct(course),
    avg_time_pct_field = mean(course_percentage_degree, na.rm = TRUE),
    TotalFieldHours    = sum(TotalFieldHours, na.rm = TRUE),
    TotalUnits         = sum(TotalUnits, na.rm = TRUE),
    Intensity          = first(Intensity),  # fixed across these courses for your data
    indep_proj_any     = as.integer(any(indep_proj_any == 1, na.rm = TRUE)),
    # carry the student’s (admission) year — first() is fine since it’s constant per student
    year               = first(year),
    # earliest field-course term’s instructor
    instructor_primary = instructor[which.min(term)],
    .groups = "drop"
  )

major_lookup <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%
  group_by(CodedID) %>%
  summarise(
    Major = first(PLNA.Plan.Descr),
    .groups = "drop"
  )

student_field_profile <- student_field_profile %>%
  left_join(major_lookup, by = "CodedID") %>%
  mutate(
    Major = factor(Major)
  )

## 6) Prior same-instructor flag (non-195 → 195)
# Build a history of non-195 instructors and see whether any of them later served as the student's BIOE 195 instructor (before the 195 term)
field_history <- enroll_field %>%
  transmute(CodedID, prior_inst = instructor, term_prior = term) %>%
  distinct()

prior_same_instructor <- field_history %>%
  inner_join(bioe195_inst, by = "CodedID") %>%
  mutate(
    match           = prior_inst == bioe195_instructor,
    prior_to_thesis = term_prior < TA.Term.Code_bioe195
  ) %>%
  group_by(CodedID) %>%
  summarise(prior_same_inst = any(match & prior_to_thesis, na.rm = TRUE), .groups = "drop")


## 7) Final student-level modeling table
# Merge thesis outcome, field-course profile, and prior-instructor flag into a single student-level dataset and construct scaled predictors

student_level <- stu_outcome %>%
  left_join(student_field_profile,  by = "CodedID") %>%
  left_join(prior_same_instructor,  by = "CodedID") %>%
  mutate(
    avg_time_pct_field_s = as.numeric(scale(avg_time_pct_field, center = TRUE, scale = TRUE)),
    TotalFieldHours_s    = as.numeric(scale(TotalFieldHours, center = TRUE, scale = TRUE)),
    TotalUnits_s         = as.numeric(scale(TotalUnits, center = TRUE, scale = TRUE)),
    prior_same_inst      = factor(replace_na(prior_same_inst, FALSE), levels = c(FALSE, TRUE)),
    year                 = as.integer(year)   # numeric year; will be factored for ICC model
  ) %>%
  # keep only students with at least one of the core field courses
  filter(!is.na(n_field_courses) & n_field_courses > 0) %>%
  distinct(CodedID, .keep_all = TRUE)

## Logistic model
#   outcome: ever completed BIOE 195 (0/1)
#   predictors: average timing of field courses, total units, independent projects, intensity

m_student_glm <- glm(
  completed_BIOE_195 ~
    avg_time_pct_field_s + Major + TotalUnits + indep_proj_any + Intensity,
  data   = student_level,
  family = binomial("logit")
)

summary(m_student_glm)
MuMIn::r.squaredGLMM(m_student_glm)   # pseudo-R²
car::vif(m_student_glm)               # check collinearity

## ICC for YEAR (null random-intercept model)
# Convert admit year to a factor and fit a null GLMM with random intercepts for year
# to estimate how much variance in thesis completion is attributable to year effects

student_level <- student_level %>%
  mutate(
    year_f = factor(year)
  )

# Random-intercept model by year (no fixed predictors)
m0_year <- lme4::glmer(
  completed_BIOE_195 ~ 1 + (1 | year_f),
  data   = student_level,
  family = binomial
)

# Extract between-year variance and compute ICC on logistic scale
var_year <- as.numeric(lme4::VarCorr(m0_year)$year_f[1])
icc_year <- var_year / (var_year + (pi^2 / 3))
cat(sprintf("ICC (year): %.4f; groups=%d\n",
            icc_year, nlevels(student_level$year_f)))

## Diagnostics (+ dispersion/zeros)
# DHARMa residual diagnostics for the logistic model:
# tests for dispersion, zero-inflation, and overall fit.
sim <- DHARMa::simulateResiduals(m_student_glm, n = 1000)
plot(sim)

## Simple visuals (sanity checks)

# Boxplot + jitter: average field-course timing vs thesis completion
p_time <- ggplot(
  student_level,
  aes(
    x = factor(completed_BIOE_195, levels = c(0, 1), labels = c("No Thesis", "Thesis Completed")),
    y = avg_time_pct_field,
    fill = factor(completed_BIOE_195, levels = c(0, 1), labels = c("No Thesis", "Thesis Completed"))
  )
) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "grey20") +
  geom_jitter(width = 0.15, alpha = 0.25, color = "grey40") +
  scale_fill_manual(values = gpa_palette) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x   = "Senior Thesis Completion",
    y   = "Average timing of field courses (% of degree)",
    fill = "Completed Thesis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 16),
    axis.title.y     = element_text(size = 20),
    axis.title.x     = element_text(size = 20),
    axis.text.x      = element_text(size = 16),
    legend.text      = element_text(size = 16),
    legend.title     = element_text(size = 20)
  ); p_time

#ggsave(
#  filename = "./drafts/figures/box_avg_timing.png",
#  plot     = p_time,
#  width    = 12, height = 7,
#  dpi      = 600, bg = "white"
#)

## ========================
## FIELD-COURSE MODELS + PER-COURSE CURVES
## ========================
# Build first-year columns for each key field course at the student level.
# For each student × course, we extract the *first* calendar year they took it,
# then pivot to a wide format so we get columns like year_BIOE_82, year_BIOE_128L, etc.
# We reuse get_year() defined earlier to convert term codes to calendar years.
all_courses <- c("BIOE 82","BIOE 128L","BIOE 141L","BIOE 151A","BIOE 155L", "BIOE 145L")

course_years_wide <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr %in% all_courses) %>%
  transmute(
    CodedID = as.character(CodedID),
    course  = gsub(" ", "_", SE.Subject.Catalog.Nbr),    # e.g., "BIOE 82" → "BIOE_82"
    year_fc = get_year(TA.Term.Code)
  ) %>%
  group_by(CodedID, course) %>%
  summarise(first_year = suppressWarnings(min(year_fc, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = course, values_from = first_year, names_prefix = "year_")

# Add first-year (per-course) variables + primary instructor to the student-level frame.
# year_* columns are converted to factors for possible use as grouping variables.
major_lookup <- time_completed_summary %>%
  mutate(CodedID = as.character(CodedID)) %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%
  arrange(CodedID, desc(TA.Term.Code)) %>%
  group_by(CodedID) %>%
  summarise(
    Major = first(PLNA.Plan.Descr),
    .groups = "drop"
  )

# Add first-year variables, instructor, and major
model_students2 <- model_students2 %>%
  mutate(
    CodedID = as.character(CodedID),
    Major = factor(Major)
  ) %>%
  left_join(course_years_wide, by = "CodedID") %>%
  mutate(across(starts_with("year_"), ~ factor(.x))) %>%
  left_join(
    student_field_profile %>%
      dplyr::distinct(CodedID, instructor_primary),
    by = "CodedID"
  )

# Per-course subsets for GLMs: restrict to students who actually took each course,
# i.e., those with non-missing timing (time_pct_* variables).
model_students_82_only   <- model_students2 %>% filter(!is.na(time_pct_BIOE_82))
model_students_128L_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_128L))
model_students_141L_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_141L))
model_students_151A_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_151A))
model_students_155L_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_155L))
model_students_145L_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_145L))

# Fit per-course logistic GLMs:
# outcome: completed_BIOE_195 (0/1),
# predictors: course-specific timing (% of degree) + GPA covariates.
glm_BIOE_82   <- glm(completed_BIOE_195 ~ time_pct_BIOE_82   + GPA_scaled + app_GPA_scaled,
                     data = model_students_82_only,   family = binomial("logit"))
glm_BIOE_128L <- glm(completed_BIOE_195 ~ time_pct_BIOE_128L + GPA_scaled + app_GPA_scaled,
                     data = model_students_128L_only, family = binomial("logit"))
glm_BIOE_141L <- glm(completed_BIOE_195 ~ time_pct_BIOE_141L + GPA_scaled + app_GPA_scaled,
                     data = model_students_141L_only, family = binomial("logit"))
glm_BIOE_151A <- glm(completed_BIOE_195 ~ time_pct_BIOE_151A + GPA_scaled + app_GPA_scaled,
                     data = model_students_151A_only, family = binomial("logit"))
glm_BIOE_155L <- glm(completed_BIOE_195 ~ time_pct_BIOE_155L + GPA_scaled + app_GPA_scaled,
                     data = model_students_155L_only, family = binomial("logit"))
glm_BIOE_145L <- glm(completed_BIOE_195 ~ time_pct_BIOE_145L + GPA_scaled + app_GPA_scaled,
                     data = model_students_145L_only, family = binomial("logit"))

# Quick summaries + DHARMa residual checks for each per-course model
summary(glm_BIOE_82);   plot(simulateResiduals(glm_BIOE_82,   n = 1000))
summary(glm_BIOE_128L); plot(simulateResiduals(glm_BIOE_128L, n = 1000))
summary(glm_BIOE_141L); plot(simulateResiduals(glm_BIOE_141L, n = 1000))
summary(glm_BIOE_151A); plot(simulateResiduals(glm_BIOE_151A, n = 1000))
summary(glm_BIOE_155L); plot(simulateResiduals(glm_BIOE_155L, n = 1000))
summary(glm_BIOE_145L); plot(simulateResiduals(glm_BIOE_145L, n = 1000))

# Helper: compute ICC for a simple null logistic GLMM with one random effect.
# Used to decide whether a clustering variable (e.g., instructor, year) has
# enough variance to warrant cluster-robust SEs.
.icc_glmm_binom <- function(data, y, cluster) {
  # Needs columns: y (0/1 or factor) and cluster (any, will be factored)
  if (!all(c(y, cluster) %in% names(data))) return(NA_real_)
  d <- data[!is.na(data[[y]]) & !is.na(data[[cluster]]), , drop = FALSE]
  if (nrow(d) < 10L) return(NA_real_)
  d[[cluster]] <- factor(d[[cluster]])
  if (nlevels(d[[cluster]]) < 2L) return(NA_real_)
  
  fit <- suppressWarnings(
    tryCatch(
      lme4::glmer(stats::as.formula(paste(y, "~ 1 + (1|", cluster, ")")),
                  data = d, family = binomial),
      error = function(e) NULL
    )
  )
  if (is.null(fit)) return(NA_real_)
  tau2 <- as.numeric(lme4::VarCorr(fit)[[cluster]][1])
  if (is.na(tau2)) return(NA_real_)
  # ICC for logistic GLMM: tau^2 / (tau^2 + pi^2/3)
  tau2 / (tau2 + (pi^2/3))
}

# ---------- ICC-aware robust coeftest (only instructor & year) ----------
# robust_ct(): run coefficient tests with cluster-robust SEs if instructor/year
# show non-trivial ICC; otherwise fall back to HC1 with row-level clustering.
robust_ct <- function(model,
                      data,
                      candidates = c("instructor_primary", "year"),
                      icc_threshold = 0.01,
                      min_levels = 2) {
  # Rows actually used by the model
  mf  <- model.frame(model)
  idx <- rownames(mf)
  
  # Align 'data' to model rows if possible; otherwise, use the model frame itself
  dat_aligned <- tryCatch(data[idx, , drop = TRUE], error = function(e) NULL)
  if (is.null(dat_aligned)) dat_aligned <- mf
  
  # Only consider clustering variables that are available in the aligned data
  cand_avail <- intersect(candidates, union(names(dat_aligned), names(mf)))
  
  # Build ICC table for each candidate clustering variable
  icc_rows <- lapply(cand_avail, function(cl_nm) {
    cl_vec <- if (cl_nm %in% names(dat_aligned)) dat_aligned[[cl_nm]] else mf[[cl_nm]]
    if (is.null(cl_vec) || length(cl_vec) != nrow(mf)) return(NULL)
    levs <- nlevels(factor(cl_vec))
    icc  <- .icc_glmm_binom(data.frame(yy = mf[[1]], cl = cl_vec), "yy", "cl")
    tibble::tibble(cluster = cl_nm, levels = levs, icc = icc)
  })
  icc_table <- dplyr::bind_rows(icc_rows)
  
  # Choose cluster with ICC ≥ icc_threshold and at least 'min_levels' levels
  pick <- icc_table %>%
    dplyr::filter(!is.na(icc), !is.na(levels), levels >= min_levels) %>%
    dplyr::arrange(dplyr::desc(icc)) %>%
    dplyr::slice_head(n = 1)
  
  if (nrow(pick) == 1L && isTRUE(pick$icc[1] >= icc_threshold)) {
    cluster_used <- pick$cluster[1]
    cl_vec <- if (cluster_used %in% names(dat_aligned)) dat_aligned[[cluster_used]] else mf[[cluster_used]]
    cl <- factor(cl_vec)
    message(sprintf("ICC check: using cluster='%s' (ICC=%.4f ≥ %.4f).",
                    cluster_used, pick$icc[1], icc_threshold))
  } else {
    # If no candidate passes the threshold, fall back to HC1 with row-level clustering.
    cluster_used <- "(row)"
    cl <- factor(seq_len(nrow(mf)))
    if (nrow(icc_table) > 0) {
      message(sprintf("ICC check: no candidate ICC ≥ %.4f (using row-level HC1).", icc_threshold))
    } else {
      message("ICC check: no candidate variables available in model rows (using row-level HC1).")
    }
  }
  
  # Cluster-robust variance–covariance matrix and coefficient tests
  Vc <- sandwich::vcovCL(model, cluster = cl, type = "HC1")
  ct <- lmtest::coeftest(model, vcov. = Vc)
  
  list(
    coeftest     = ct,
    vcov         = Vc,
    cluster_used = cluster_used,
    icc_table    = icc_table
  )
}

# Run robust_ct for each course-level model and inspect ICC / robust tests
res_82   <- robust_ct(glm_BIOE_82,   model_students_82_only)   # use these results when reporting
res_128L <- robust_ct(glm_BIOE_128L, model_students_128L_only)
res_141L <- robust_ct(glm_BIOE_141L, model_students_141L_only)
res_151A <- robust_ct(glm_BIOE_151A, model_students_151A_only)
res_155L <- robust_ct(glm_BIOE_155L, model_students_155L_only)
res_145L <- robust_ct(glm_BIOE_145L, model_students_145L_only)

res_82$icc_table;   res_82$coeftest;   res_82$cluster_used
res_128L$icc_table; res_128L$coeftest; res_128L$cluster_used
res_151A$icc_table; res_151A$coeftest; res_151A$cluster_used
res_155L$icc_table; res_155L$coeftest; res_155L$cluster_used
res_145L$icc_table; res_145L$coeftest; res_145L$cluster_used



## =========================
## SAVE ANALYSIS OBJECTS
## =========================
# These objects are useful for a separate 02_figures_and_supp_tables.R script.
saveRDS(bioe_students, here::here("outputs", "objects", "bioe_students.rds"))
saveRDS(students_with_status, here::here("outputs", "objects", "students_with_status.rds"))
saveRDS(students_with_status_eeb, here::here("outputs", "objects", "students_with_status_eeb.rds"))
saveRDS(time_completed_summary, here::here("outputs", "objects", "time_completed_summary.rds"))
saveRDS(student_flags, here::here("outputs", "objects", "student_flags.rds"))
saveRDS(all_course_results, here::here("outputs", "objects", "all_course_results.rds"))
saveRDS(model_students2, here::here("outputs", "objects", "model_students2.rds"))
saveRDS(model_df, here::here("outputs", "objects", "model_df.rds"))
saveRDS(overall_gap_tests, here::here("outputs", "objects", "overall_gap_tests.rds"))
saveRDS(gap_trend_models, here::here("outputs", "objects", "gap_trend_models.rds"))
saveRDS(multi_mod_all_courses, here::here("outputs", "objects", "multi_mod_all_courses.rds"))
