
## =========================
## HOUSEKEEPING
## =========================
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
  library(MuMIn)      # r.squaredGLMM
  library(DHARMa)     
  library(brglm)     
  library(nnet)       # multinom
  library(sandwich)   
  library(lmtest)     
  library(emmeans)  
})

theme_set(theme_minimal())
setwd("~/Documents/PhD Documents/Senior Thesis Project")

## =========================
## PATHS & DATA
## =========================
degree             <- read.csv("data/Degree.csv",      header=TRUE, sep=",")
non_degree         <- read.csv("data/Non-Degree.csv", header=TRUE, sep=",")
quarter_performance<- read.csv("data/QuarterPerformance.csv", header=TRUE, sep=",")
quarter_info       <- read.csv("data/QuarterInfo_v2_feb5.csv", header=TRUE, sep=",")
class_performance  <- read.csv("data/ClassPerformance_v2_apr22.csv", header=TRUE, sep=",")
offerings          <- read.csv("data/Offerings.csv", header=TRUE, sep=",")
field_course_info <- read.csv("data/fieldcourse_info_sep10.csv", header = TRUE, sep=",")

## =========================
## CONSTANTS & PALETTES
## =========================
majors_keep <- c("Ecology and Evolution BS","Marine Biology BS","Biology BS","Plant Sciences BS", "Environmental Science BS")

# Field courses of interest (excluding 195 when counting “field courses taken”)
field_courses_core <- c(
  "BIOE 82","BIOE 85","BIOE 128L","BIOE 141L","BIOE 151A",
  "BIOE 153A","BIOE 155L","BIOE 157A","BIOE 159A","BIOE 161L",
  "BIOE 150L","ENVS 107A","ENVS 104A")

# “All courses” list (includes 195) used for row flags if needed elsewhere
all_courses <- c("BIOE 20F","BIOE 82","BIOE 85","BIOE 128L","BIOE 141L","BIOE 155L","BIOE 158L",
                 "BIOE 145L","BIOE 157A","BIOE 161L","BIOE 159A","BIOE 150L","BIOE 151A",
                 "ENVS 107A","BIOE 153A","BIOE 195","ENVS 104A")

# Consistent palette by semantic value (used across multiple figures)
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
  "Asian"="#e9c46a","Hispanic/Latino"="#a65211","Native Hawaiian/Oth Pac Island"="#81400e",
  "Other"="#2a9d8f","Unknown/Not Specified"="#76c893","White/Caucasian"="#264653"
)

gpa_palette   <- c("0"="#0a9396","1"="#94d2bd")
field_palette <- c("#540b0e","#bb3e03","#ca6702","#ee9b00","#fff3b0")
thesis_yes_no_palette <- c("Yes" = "#0a9396", "No" = "#94d2bd")

## =========================
## HELPERS
## =========================
# Convert a term code (YYYQ or YYYYQ) to calendar year.
# If 4 chars (e.g., 2238), year = 1800 + first 3 digits. If 5 chars (e.g., 20238), year = first 4.
get_year <- function(tc) {
  s <- as.character(tc); n <- nchar(s)
  y4 <- suppressWarnings(as.integer(substr(s, 1, 4)))
  y3 <- suppressWarnings(as.integer(substr(s, 1, 3))) + 1800L
  ifelse(n == 5L, y4, ifelse(n == 4L, y3, NA_integer_))
}

#Map the last char to quarter index (Fall=8→0, Winter=0→1, Spring=2→2, Summer=4→3)
# Using 0–3 lets us build a linear “term index” (year*4 + quarter_index).
get_quarter <- function(tc) {
  s <- as.character(tc); q <- substr(s, nchar(s), nchar(s))
  dplyr::case_when(q == "8" ~ 0L, q == "0" ~ 1L, q == "2" ~ 2L, q == "4" ~ 3L, TRUE ~ NA_integer_)
}

# Linear term index used for time differences across terms (e.g., admit→grad).
get_term_index <- function(tc) {
  y <- get_year(tc); qi <- get_quarter(tc)
  ifelse(is.na(y)|is.na(qi), NA_integer_, y*4L + qi)
}

# One-decimal percent labels
percent1 <- function(x) scales::percent(x, accuracy = 1)

## =========================
## BUILD PER-ROW COURSE FLAGS & MERGE
## =========================
# Create “completed_<course>” Yes/No flags on each *row* of class_performance.
# For each row: if SE.Subject.Catalog.Nbr equals that course, flag “Yes”, else “No”.
# (Downstream we aggregate these per student to build “ever took course” indicators.)
final_class_performance <- purrr::reduce(
  all_courses,
  .init = class_performance,
  .f = \(df, course) {
    nm <- paste0("completed_", gsub(" ", "_", course))
    dplyr::mutate(df, !!nm := dplyr::if_else(SE.Subject.Catalog.Nbr == course, "Yes", "No"))
  }
)

# Merge quarter metadata onto performance using (CodedID, TA.Term.Code)
quarter_data <- left_join(
  quarter_info,
  quarter_performance,
  by = c("CodedID","TA.Term.Code")
)

# Merge degree info onto quarter_data (by CodedID and major), keep TA.Term.Code as character
degree_quarter <- left_join(
  degree,
  quarter_data,
  by = c("CodedID","PLNA.Plan.Descr")
) %>%
  mutate(TA.Term.Code = as.character(TA.Term.Code))

# Attach row-level completion flags to degree/term table by (CodedID, TA.Term.Code).
# This yields a long table with per-term attributes AND “completed_*” flags for that same term.
bioe_195_final <- left_join(
  degree_quarter,
  final_class_performance %>% mutate(TA.Term.Code = as.character(TA.Term.Code)),
  by = c("CodedID","TA.Term.Code")
)

# Keep only target majors + nonzero term units (dropped W/placeholder rows)
bioe_students <- bioe_195_final %>%
  filter(
    PLNA.Plan.Descr %in% majors_keep,
    Term_Units_Earned != 0
  )

## =========================
## FIELD-COURSE ATTRIBUTES & JOIN
## =========================
# Normalize types & names for field-course metadata to join cleanly on course code
field_lu <- field_course_info %>%
  transmute(
    SE.Subject.Catalog.Nbr = str_trim(as.character(SE.Subject.Catalog.Nbr)),
    TotalFieldHours        = as.numeric(Total.field.hours),
    Independent.projects   = str_trim(as.character(Independent.projects)),
    TotalUnits             = as.numeric(Units),
    StudentsinClass        = as.numeric(Students.per.Quarter),
    Intensity              = as.factor(Intensity)
  )

# Attach course attributes to the long per-row student/enrollment table
bioe_students <- bioe_students %>%
  mutate(SE.Subject.Catalog.Nbr = str_trim(as.character(SE.Subject.Catalog.Nbr))) %>%
  left_join(field_lu, by = "SE.Subject.Catalog.Nbr")

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

# Helper: base code without a single trailing A/B/C/D (keeps "L")
base_code <- function(x) sub("([0-9]{2,3})[A-D]$", "\\1", x)

bioe_courses <- bioe_students %>%
  filter(SE.Subject.Catalog.Nbr %in% all_courses) %>%
  distinct(CodedID, SE.Subject.Catalog.Nbr) %>%
  transmute(
    Course = str_squish(as.character(SE.Subject.Catalog.Nbr)),
    Course_base = base_code(Course)
  ) %>%
  count(Course, Course_base, name = "Students") %>%
  # add readable labels
  left_join(course_names, by = "Course") %>%
  mutate(CourseLabel = dplyr::coalesce(CourseLabel, Course)) %>%
  # attach Intensity: try exact code, then fall back to base code if missing
  left_join(field_lu %>% transmute(Course = SE.Subject.Catalog.Nbr, Intensity), by = "Course") %>%
  mutate(
    Intensity = dplyr::coalesce(
      Intensity,
      field_lu$Intensity[match(Course_base, field_lu$SE.Subject.Catalog.Nbr)]
    ),
    Intensity = dplyr::case_when(Course == "BIOE 195" ~ "Research", TRUE ~ as.character(Intensity)),
    Intensity = factor(Intensity, levels = c("Introductory","Low intensity","Medium intensity","Intensive","Research"))
  )

# quick sanity (optional):
# stopifnot(!any(is.na(bioe_courses$CourseLabel)))

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
  )

field_course_count

ggsave(
  filename = "./drafts/Figures/number_of_field_courses.png",
  plot     = field_course_count,
  width    = 8, height = 10, dpi = 600,
  bg = 'white'
)


# Keep the latest term per student and convert to academic year.
# Calculation: Academic year equals calendar year unless the term is Fall (quarter==8),
# in which case the academic year is next calendar year.
# --------------------------------
latest_term <- bioe_students %>%
  group_by(CodedID) %>%
  filter(TA.Term.Code == max(TA.Term.Code, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    year_raw      = as.numeric(substr(TA.Term.Code, 1, 3)) + 1800,
    quarter       = as.numeric(substr(TA.Term.Code, 4, 4)),
    academic_year = if_else(quarter == 8, year_raw + 1, year_raw)
  ) %>%
  distinct(CodedID, .keep_all = TRUE)

thesis_yes_ids <- bioe_students %>%
  dplyr::filter(completed_BIOE_195 == "Yes") %>%
  dplyr::distinct(CodedID) %>%
  dplyr::mutate(completed_thesis = "Yes")

students_with_status <- latest_term %>%
  dplyr::left_join(thesis_yes_ids, by = "CodedID") %>%
  dplyr::mutate(
    completed_thesis = dplyr::if_else(is.na(completed_thesis), "No", completed_thesis),
    completed_thesis = factor(completed_thesis, levels = c("No", "Yes"))
  ) %>%
  dplyr::distinct(CodedID, .keep_all = TRUE)  # <- one row per student

# Yearly counts and proportions (per-student)
yearly_counts <- students_with_status %>%
  dplyr::count(academic_year, completed_thesis, name = "n") %>%
  dplyr::group_by(academic_year) %>%
  dplyr::mutate(total = sum(n), prop = n / total) %>%
  dplyr::ungroup() %>%
  filter(academic_year != 2025)

year_totals <- yearly_counts %>%
  dplyr::distinct(academic_year, total)

# Plot
p_basic_dist <- ggplot(yearly_counts,
                       aes(x = factor(academic_year),
                           y = prop,
                           fill = completed_thesis)) +
  geom_col(color = NA) +
  geom_text(data = year_totals,
            aes(x = factor(academic_year), y = 1, label = total),
            vjust = -0.6, size = 5,
            inherit.aes = FALSE) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = thesis_yes_no_palette,
                    labels = c("Yes" = "Thesis Completed", "No" = "No Thesis")) +
  labs(x = "Academic Year", y = "Percentage of Students", fill = "Completed Thesis") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "right",
        plot.margin     = margin(t = 10, r = 5, b = 5, l = 5),
        axis.text  = element_text(size = 16),
        axis.title = element_text(size = 22))

p_basic_dist



###### DROPPED VS. TAKEN #######
bioe195_attempts <- bioe_students %>%
  filter(SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  transmute(
    CodedID,
    TA.Term.Code = as.character(TA.Term.Code),
    status = toupper(trimws(as.character(`SE.Student.Enrollment.Status.Code`)))
  ) %>%
  # de-dupe in case multiple rows per student-term exist after joins
  distinct(CodedID, TA.Term.Code, status)

# Quick sanity check (optional)
# with(bioe195_attempts, table(status, useNA = "ifany"))

# 2) Attempt-year (from TA.Term.Code) for time slicing
bioe195_attempts <- bioe195_attempts %>%
  mutate(
    year_raw = as.numeric(substr(TA.Term.Code, 1, 3)) + 1800,
    quarter  = as.numeric(substr(TA.Term.Code, 4, 4)),
    acad_year_attempt = dplyr::if_else(quarter == 8, year_raw + 1, year_raw),
    dropped  = status == "D",
    enrolled = status == "E"
  )

# 3) Per-student rollup (ever logic)
bioe195_by_student <- bioe195_attempts %>%
  group_by(CodedID) %>%
  summarise(
    ever_took    = any(enrolled | dropped, na.rm = TRUE),
    ever_dropped = any(dropped, na.rm = TRUE),
    ever_enrolled= any(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(ever_took)

# 4) Overall proportion dropped among takers
overall_195 <- bioe195_by_student %>%
  summarise(
    n_took    = n(),
    n_dropped = sum(ever_dropped),
    prop_dropped_among_takers = n_dropped / n_took
  )
print(overall_195)

# 5) By attempt year (a student can appear in multiple years if multiple attempts)
by_year_attempt <- bioe195_attempts %>%
  filter(enrolled | dropped) %>%
  distinct(CodedID, acad_year_attempt, status) %>%
  group_by(acad_year_attempt) %>%
  summarise(
    n_took     = n_distinct(CodedID),
    n_dropped  = n_distinct(CodedID[status == "D"]),
    prop_dropped = if_else(n_took > 0, n_dropped / n_took, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(acad_year_attempt)
print(by_year_attempt)

# 6) (Optional) Anchor to each student's latest academic year instead
by_year_latest <- latest_term %>%                     # you already built this
  select(CodedID, academic_year) %>%
  inner_join(bioe195_by_student, by = "CodedID") %>%
  group_by(academic_year) %>%
  summarise(
    n_took     = n(),
    n_dropped  = sum(ever_dropped),
    prop_dropped = n_dropped / n_took,
    .groups = "drop"
  ) %>%
  arrange(academic_year)
print(by_year_latest)

## =========================
## OPTIONAL: plot drop rate by attempt year
## =========================
library(ggplot2)
p_195_drop_rate <- ggplot(by_year_attempt,
                          aes(x = factor(acad_year_attempt), y = prop_dropped)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Academic Year of BIOE 195 Attempt",
       y = "Drop rate among BIOE 195 takers (unique students)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_195_drop_rate

ggsave( filename = "./drafts/figures/p_basic_dist.png", plot = p_basic_dist, width = 10, height = 5, dpi = 600, bg = "white")


# Prepare a clean frame for gaps: recode column names and drop unknown levels
dems <- c("Gender","Race/Ethnicity","Status","Admit Type","First Generation","EOP")
levels_to_drop <- c("Unknown","-", "Unknown/Other", "Senior Transfer", "Other", "U")

# (NEW) Define thesis_yes_ids_eeb (needed for the left_join below)
thesis_yes_ids_eeb <- bioe_students %>%
  filter(completed_BIOE_195 == "Yes") %>%
  distinct(CodedID) %>%
  mutate(completed_thesis = "Yes")

latest_term_eeb <- bioe_students %>%
  group_by(CodedID) %>%
  filter(TA.Term.Code == max(TA.Term.Code, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    year_raw      = as.numeric(substr(TA.Term.Code, 1, 3)) + 1800,
    quarter       = as.numeric(substr(TA.Term.Code, 4, 4)),
    academic_year = if_else(quarter == 8, year_raw + 1, year_raw)
  )

students_with_status_eeb <- latest_term_eeb %>%
  left_join(thesis_yes_ids_eeb, by = "CodedID") %>%   # <-- unchanged (now defined above)
  mutate(
    completed_thesis = if_else(is.na(completed_thesis), "No", completed_thesis),
    completed_thesis = factor(completed_thesis, levels = c("No","Yes"))
  )

bioe_students_clean <- latest_term_eeb %>%
  left_join(
    students_with_status_eeb %>% select(CodedID, completed_thesis),
    by = "CodedID"
  ) %>%
  rename(
    "Gender"         = "PD.Gender...Current",
    "Race/Ethnicity" = "ET.Ethnic.Group.Category",
    "Status"         = "PD.Domestic.International...Current",
    "Admit Type"     = "UAD.Admit.Type.Descr",
    "First Generation"      = "UAD.First.Generation",
    "EOP"            = "SGC.Is.EOP"
  ) %>%
  select(CodedID, academic_year, completed_thesis, all_of(dems)) %>%
  mutate(across(all_of(dems), ~ forcats::as_factor(as.character(.x)))) %>%
  droplevels() %>%
  distinct(CodedID, .keep_all = TRUE)

# --------------------------------
# c) Per-dimension level drop (unchanged logic; one safety line added)
# --------------------------------
compute_gap <- function(df, by_year = FALSE) {
  res <- if (by_year) df %>% rename(Year = academic_year) else df %>% mutate(Year = NA_integer_)
  purrr::map_dfr(dems, function(var) {
    res %>%
      # drop unknown/other ONLY for this variable
      mutate(value_raw = trimws(as.character(.data[[var]]))) %>%
      filter(!(value_raw %in% levels_to_drop)) %>%
      group_by(Year, value = forcats::as_factor(value_raw), completed_thesis) %>%
      summarise(n = n(), .groups = "drop") %>%
      # >>> ensure both No/Yes rows exist for every (Year, value)
      tidyr::complete(
        Year, value,
        completed_thesis = factor(c("No","Yes"), levels = c("No","Yes")),
        fill = list(n = 0L)
      ) %>%
      tidyr::pivot_wider(names_from = completed_thesis, values_from = n, names_prefix = "Thesis") %>%
      mutate(
        ThesisYes = tidyr::replace_na(ThesisYes, 0L),
        ThesisNo  = tidyr::replace_na(ThesisNo,  0L),
        Total     = ThesisYes + ThesisNo
      ) %>%
      group_by(Year) %>%
      mutate(
        DemRate    = Total / sum(Total),
        tot_yes    = sum(ThesisYes),
        ThesisRate = if_else(tot_yes > 0, ThesisYes / tot_yes, NA_real_),
        ThesisGap  = ThesisRate - DemRate
      ) %>%
      ungroup() %>%
      select(-tot_yes) %>%
      mutate(Demographic = var)
  }) %>%
    rename(Value = value)
}

# Overall gaps (collapsing years) and yearly gaps
agg_all    <- compute_gap(bioe_students_clean, by_year = FALSE)
agg_all_yr <- compute_gap(bioe_students_clean, by_year = TRUE)

# Overall gap bars (x = ThesisGap), one facet per demographic
p_gap <- ggplot(agg_all, aes(y = Value, x = ThesisGap, fill = Value)) +
  geom_col(show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = label_percent(style_positive = "plus")) +
  scale_fill_manual(values = color_palette) +
  labs(x = "Participation Gap (Thesis − Rep.)", y = NULL) +
  ggthemes::theme_few(base_size = 15) +
  facet_grid(Demographic ~ ., scales = "free_y", space = "free", switch = "y") + 
  theme(axis.text  = element_text(size = 17),
        axis.title = element_text(size = 22))

# Side labels: show ThesisRate vs DemRate values as percentages
labels_long <- agg_all %>%
  transmute(Demographic, Value, `Thesis` = ThesisRate, `Rep.` = DemRate) %>%
  pivot_longer(c(`Thesis`,`Rep.`), names_to = "Metric", values_to = "pct") %>%
  mutate(Metric = factor(Metric, levels = c("Thesis","Rep.")))

p_pct <- ggplot(labels_long, aes(y = Value, x = Metric, label = percent1(pct))) +
  geom_text(size = 5, color = "grey15") +
  scale_x_discrete(position = "top") +
  facet_grid(Demographic ~ ., scales = "free_y", space = "free", switch = "y") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y.left = element_blank(),
        axis.text  = element_text(size = 13))

# Combine gap bars + side percentage labels
p_gap_overall <- (p_gap + p_pct) + patchwork::plot_layout(widths = c(4, 1))
p_gap_overall

ggsave(
  filename = "./drafts/Figures/gap_figure_overall.png",
  plot     = p_gap_overall,
  width    = 10, height = 9, dpi = 600,
  bg = 'white'
)

test_gaps_overall <- function(agg_all) {
  # agg_all: from compute_gap(..., by_year = FALSE)
  agg_all %>%
    group_by(Demographic) %>%
    summarise(
      # successes = ThesisYes; totals = Total per level
      chisq = {
        x <- ThesisYes
        n <- Total
        if (length(x) >= 2 && all(is.finite(x)) && all(is.finite(n)) && all(n > 0)) {
          suppressWarnings(prop.test(x = x, n = n)$statistic)
        } else NA_real_
      },
      df = {
        k <- n()  # number of levels in this Demographic
        if (k >= 2) k - 1 else NA_integer_
      },
      p_value = {
        x <- ThesisYes
        n <- Total
        if (length(x) >= 2 && all(is.finite(x)) && all(is.finite(n)) && all(n > 0)) {
          suppressWarnings(prop.test(x = x, n = n)$p.value)
        } else NA_real_
      },
      N = sum(Total, na.rm = TRUE),
      k_levels = n(),
      .groups = "drop_last"
    ) %>%
    mutate(
      cramer_v = {
        # Cramér’s V from chi-square
        # Note: min(r-1, c-1) with r = k_levels, c = 2 (Yes/No) => min(k-1, 1) = 1
        ifelse(is.finite(chisq) & is.finite(N) & N > 0,
               sqrt(as.numeric(chisq) / (N * 1)),  # denominator becomes N * 1
               NA_real_)
      }
    ) %>%
    ungroup() %>%
    mutate(p_adj = p.adjust(p_value, method = "BH")) %>%
    arrange(p_value)
}

overall_gap_tests <- test_gaps_overall(agg_all)
overall_gap_tests

# Year-by-year gap lines per demographic
demo_cross_years <- ggplot(agg_all_yr, aes(y = ThesisGap, x = Year, col = Value)) +
  geom_point(show.legend = FALSE, size = 4) +
  geom_path(show.legend = FALSE, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = label_percent(style_positive = "plus")) +
  scale_x_continuous(breaks = 2011:2024, expand = expansion(mult = c(0.25, 0))) +
  scale_color_manual(values = color_palette) +
  labs(y = "Participation Gap", x = "Year") +
  ggthemes::theme_few(base_size = 20) +
  theme(
    axis.text.x  = element_text(size = 16, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y  = element_text(size = 16),
    axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 12)),
    axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 12)),
    strip.text   = element_text(size = 20, face = "bold"),
    plot.title   = element_text(size = 24, face = "bold", hjust = 0.5),
    panel.grid   = element_line(color = "grey85", linewidth = 0.3)  # ✅ fixed here
  ) +
  facet_wrap(~ Demographic)

demo_cross_years

ggsave(
  filename = "./drafts/demo_across_years.png",
  plot     = demo_cross_years,
  width    = 15, height = 8, dpi = 600,
  bg = 'white'
)

gap_trend <- agg_all_yr %>%
  mutate(abs_gap = abs(ThesisGap)) %>% 
  group_by(Demographic, Year) %>%
  summarise(mean_abs_gap = mean(abs_gap, na.rm = TRUE),
            .groups = "drop")

# Test if mean absolute gap is changing over time
gap_trend_models <- gap_trend %>%
  group_by(Demographic) %>%
  summarise(
    slope = coef(lm(mean_abs_gap ~ Year))[2],
    p_value = summary(lm(mean_abs_gap ~ Year))$coefficients[2, 4],
    r2 = summary(lm(mean_abs_gap ~ Year))$r.squared
  )

gap_trend_models

## =========================
## FIELD-COURSE COUNT DISTRIBUTION
## =========================
# For each student, count distinct field courses taken (from field_courses_core).
# Missing counts (no field courses) are set to 0.
# Total field-course UNITS per student
field_counts <- bioe_students %>%
  distinct(CodedID) %>%
  left_join(
    bioe_students %>%
      filter(SE.Subject.Catalog.Nbr %in% field_courses_core) %>%
      distinct(CodedID, SE.Subject.Catalog.Nbr, TotalUnits) %>%   # one row per student-course
      group_by(CodedID) %>%
      summarise(field_units = sum(TotalUnits, na.rm = TRUE), .groups = "drop"),
    by = "CodedID"
  ) %>%
  mutate(
    field_units = replace_na(field_units, 0),
    field_units = pmin(field_units, 20)   # cap at 20
  )

# Bin field_units
field_counts <- field_counts %>%
  mutate(
    field_bin = case_when(
      field_units == 0 ~ "0",
      field_units >= 2  & field_units <= 5  ~ "2–5",
      field_units >= 6  & field_units <= 10 ~ "6–10",
      field_units >= 11 & field_units <= 15 ~ "11–15",
      field_units >= 16 & field_units <= 20 ~ "16–20"
    ),
    field_bin = factor(field_bin, levels = c("0", "2–5", "6–10", "11–15", "1–20"))
  )

# Distribution of binned units
field_dist <- field_counts %>%
  count(field_bin, name = "n_students") %>%
  filter(!is.na(field_bin)) %>%            # Remove any gaps
  mutate(
    prop = n_students / sum(n_students),
    pct  = percent1(prop)
  )

# Colors for bins
bin_levels <- levels(field_counts$field_bin)
field_cols <- set_names(rep_len(field_palette, length(bin_levels)), bin_levels)

# Plot
p_field_dist <- ggplot(field_dist,
                       aes(x = field_bin,
                           y = n_students,
                           fill = field_bin)) +
  geom_col(color = "grey70", linewidth = 0.3) +
  geom_text(aes(label = pct), vjust = -0.5, size = 4, color = "grey20") +
  scale_fill_manual(values = field_cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Total field-course units (binned)", y = "# Students") +
  theme(panel.grid.minor = element_blank())

p_field_dist
## =====================================================
## A) MULTINOMIAL MODEL FOR FIELD COURSE IMPACTS
## =====================================================
# Build student-level “ever took” flags from the row-level Yes/No flags created earlier.
# This collapses per-row flags into per-student indicators (1 if ever “Yes” across rows).
# Build student-level “ever took” flags from the row-level Yes/No flags created earlier.
mn_flag_cols <- paste0("completed_", gsub(" ", "_", field_courses_core))

student_flags <- bioe_students %>%
  group_by(CodedID) %>%
  filter() %>%
  summarise(
    completed_BIOE_195 = any(completed_BIOE_195 == "Yes", na.rm = TRUE),
    across(all_of(mn_flag_cols), ~ any(. == "Yes", na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("completed_"), as.integer))  # TRUE/FALSE → 1/0

# Predictors are the per-course flags; outcome is thesis completion 0/1.
course_cols <- mn_flag_cols[!grepl("BIOE_153A", mn_flag_cols)]
  
student_flags <- student_flags %>%
  mutate(completed_BIOE_195 = factor(completed_BIOE_195, levels = c(0, 1))) %>%
  distinct(CodedID, .keep_all = TRUE)

# Fit a (binary) logistic model via multinom()
model_formula <- reformulate(termlabels = course_cols, response = "completed_BIOE_195")
multi_mod_all_courses <- nnet::multinom(model_formula, data = student_flags, trace = FALSE)
sm <- summary(multi_mod_all_courses)

tidy_multinom <- function(mod) {
  sm <- summary(mod)
  coefs <- sm$coefficients
  ses   <- sm$standard.errors
  
  # Binary case: vectors → make a 1-row matrix and keep names
  if (is.null(dim(coefs))) {
    nm <- names(coefs)
    z  <- coefs / ses
    p  <- 2 * (1 - pnorm(abs(z)))
    
    out <- data.frame(
      class    = if (!is.null(rownames(coefs))) "1" else "1",
      course   = nm,
      estimate = as.numeric(coefs),
      se       = as.numeric(ses),
      z        = as.numeric(z),
      p_value  = as.numeric(p),
      row.names = NULL
    )
  } else {
    # True multinomial (≥3 classes)
    z <- coefs / ses
    p <- 2 * (1 - pnorm(abs(z)))
    
    out <- do.call(rbind, lapply(seq_len(nrow(coefs)), function(i) {
      data.frame(
        class    = rownames(coefs)[i],
        course   = colnames(coefs),
        estimate = as.numeric(coefs[i, ]),
        se       = as.numeric(ses[i, ]),
        z        = as.numeric(z[i, ]),
        p_value  = as.numeric(p[i, ]),
        row.names = NULL
      )
    }))}
}
  
results_table <- tidy_multinom(multi_mod_all_courses)
results_table
  
# Compute “average predicted probabilities” (AME-style)
compute_toggle_probs <- function(model, data, cols, outcome = "1") {
  get_prob1 <- function(nd) {
    p <- predict(model, newdata = nd, type = "probs")
    if (is.matrix(p) && !is.null(colnames(p)) && outcome %in% colnames(p)) {
      as.numeric(p[, outcome, drop = TRUE])
    } else if (is.matrix(p)) {
      as.numeric(p[, ncol(p), drop = TRUE])
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

ame_tbl <- compute_toggle_probs(multi_mod_all_courses, student_flags, course_cols, outcome = "1") %>%
  mutate(
    Course = gsub("_", " ", sub("^completed_", "", course_flag)),
    d_pp   = (p1 - p0) * 100,
    rr     = ifelse(p0 > 0, p1 / p0, NA_real_)
  )

# Odds ratios + Wald CIs from the model
or_tbl <- broom::tidy(multi_mod_all_courses, conf.int = TRUE) %>%
  filter(term %in% course_cols) %>%
  transmute(course_flag = term,
            OR = exp(estimate),
            OR_low = exp(conf.low), OR_high = exp(conf.high),
            p.value)

# Combine, then filter for significance (α = 0.05)
results <- ame_tbl %>%
  left_join(or_tbl, by = "course_flag") %>%
  dplyr::select(Course, p0, p1, d_pp, rr, OR, OR_low, OR_high, p.value) %>%
  arrange(desc(d_pp)) %>%
  filter(!is.na(p.value), p.value < 0.05)

# Observed proportions per course (only for significant ones)

## =====================================================
## A) PLOT FIELD COURSE RESULTS - SIGNIFICANT ONLY
## =====================================================

#Filter out only significant courses 
alpha <- 0.05
results_sig <- results %>%
  dplyr::filter(!is.na(p.value), p.value < alpha)

#Code observed proportions per course + no field course group 
sf <- student_flags %>%
  mutate(
    CodedID = as.character(CodedID),
    Thesis  = ifelse(as.integer(as.character(completed_BIOE_195)) == 1, "Yes", "No")
  )

#Observed counts for each field course (Yes/No completion)
overall_counts <- purrr::map_dfr(course_cols, function(col) {
  takers <- sf %>% dplyr::filter(.data[[col]] %in% c(1,"1",TRUE,"Yes"))
  tibble::tibble(Course = gsub("_", " ", sub("^completed_", "", col))) %>%
    dplyr::bind_cols(
      takers %>%
        dplyr::count(Thesis, name = "n") %>%
        tidyr::complete(Thesis = c("Yes","No"), fill = list(n = 0))
    )
})

# Add “No field course”
no_fc_counts <- sf %>%
  mutate(any_fc = rowSums(across(all_of(course_cols), ~ as.integer(. %in% c(1,"1",TRUE,"Yes"))), na.rm = TRUE)) %>%
  dplyr::filter(any_fc == 0L) %>%
  dplyr::count(Thesis, name = "n") %>%
  tidyr::complete(Thesis = c("Yes","No"), fill = list(n = 0)) %>%
  mutate(Course = "No field course", .before = 1)

overall_counts <- dplyr::bind_rows(overall_counts, no_fc_counts)

# Keep only significant courses + the “No field course” bar
keep_courses <- c("No field course", results_sig$Course)
overall_counts_sig <- overall_counts %>%
  dplyr::filter(Course %in% keep_courses)

# proportions for stacked bar
overall_props_sig <- overall_counts_sig %>%
  dplyr::group_by(Course) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

# totals for labels
totals_sig <- overall_counts_sig %>%
  dplyr::group_by(Course) %>%
  dplyr::summarise(total = sum(n), .groups = "drop")

# order bars by observed “Yes” proportion (desc), still includes “No field course”
order_levels_sig <- overall_props_sig %>%
  dplyr::filter(Thesis == "Yes") %>%
  dplyr::arrange(dplyr::desc(prop)) %>%
  dplyr::pull(Course)

#Predicted probability labels only for significant courses
overlay_probs_sig <- results_sig %>%
  dplyr::filter(Course %in% order_levels_sig) %>%
  dplyr::mutate(
    Course      = factor(Course, levels = order_levels_sig),
    p0          = pmin(pmax(p0, 0), 1),
    p1          = pmin(pmax(p1, 0), 1),
    label_range = paste0(scales::percent(p0, accuracy = 0.1), "–",
                         scales::percent(p1, accuracy = 0.1))
  )

course_names <- tribble(
  ~Course,     ~CourseLabel,
  "BIOE 82",   "Intro Field Research",
  "BIOE 128L", "Large Marine Vert.",
  "BIOE 141L", "Behavioral Eco.",
  "BIOE 155L", "Freshwater Eco. Lab",
  "BIOE 151A", "Eco. & Conservation"
)

# Helper to add CourseLabel, preserving "No field course"
add_course_labels <- function(df) {
  df %>%
    mutate(Course = as.character(Course)) %>%
    left_join(course_names, by = "Course") %>%
    mutate(CourseLabel = dplyr::coalesce(
      CourseLabel,
      if_else(Course == "No field course", "No field course", Course)
    ))
}

# Label all data frames used in the plot
overall_props_sig_lab <- add_course_labels(overall_props_sig)
totals_sig_lab        <- add_course_labels(totals_sig)
overlay_probs_sig_lab <- add_course_labels(overlay_probs_sig)

# Translate your ordering vector into labeled levels
order_levels_sig_lab <- tibble(Course = as.character(order_levels_sig)) %>%
  left_join(course_names, by = "Course") %>%
  mutate(CourseLabel = dplyr::coalesce(
    CourseLabel,
    if_else(Course == "No field course", "No field course", Course)
  )) %>%
  pull(CourseLabel)

# --- Plot with labels ---
yes_per_field_course_with_preds <- ggplot(
  overall_props_sig_lab,
  aes(x = factor(CourseLabel, levels = order_levels_sig_lab),
      y = prop, fill = Thesis)
) +
  geom_col(width = 0.8, color = "white") +
  geom_text(
    data = overall_props_sig_lab %>% dplyr::filter(prop >= 0.05),
    aes(label = scales::percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 6, show.legend = FALSE
  ) +
  geom_text(
    data = totals_sig_lab,
    aes(x = factor(CourseLabel, levels = order_levels_sig_lab),
        y = 1.05, label = paste0("n=", total)),
    inherit.aes = FALSE, size = 5
  ) +
  # predicted probability range (only for sig courses; none for "No field course")
  geom_text(
    data = overlay_probs_sig_lab,
    aes(x = factor(CourseLabel, levels = order_levels_sig_lab),
        y = 1.10, label = label_range),
    inherit.aes = FALSE, size = 3.7, fontface = "italic"
  ) +
  scale_fill_manual(
    values = thesis_yes_no_palette,
    labels = c("Yes" = "Thesis Completed", "No" = "No Thesis")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.15),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Course", y = "Percentage of students", fill = "Completed Thesis") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y  = element_text(size = 16),
    axis.title   = element_text(size = 20),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 15),
    panel.grid.minor = element_blank()
  )

yes_per_field_course_with_preds

# Save the field-course proportions + predicted range figure
ggsave(
  filename = "./drafts/Figures/yes_per_field_course.png",
  plot     = yes_per_field_course_with_preds,
  width    = 9, height = 6, units = "in",
  dpi      = 600, bg = "white"
)

## =========================
## DEGREE-TIMING & FIELD-COURSES
## =========================
# Build per-row timing features relative to each student's admit→grad window.
# - denom: # of quarter-steps from admit to grad (min 1 to avoid /0)
# - raw_pct: where a course term falls between admit and grad
# - course_percentage_degree: clamp raw_pct to [0,1] for clean summaries
time_completed_summary <- bioe_students %>%
  mutate(
    admit_term  = get_term_index(UAD.Admit.Term.Code),
    course_term = get_term_index(TA.Term.Code),
    grad_term   = get_term_index(DG.Completion.Term.Code),
    denom = if_else(
      is.na(admit_term) | is.na(grad_term),
      NA_integer_,
      pmax(1L, grad_term - admit_term)
    ),
    raw_pct = (course_term - admit_term) / denom,
    course_percentage_degree = pmin(pmax(raw_pct, 0), 1)
  )

# Timing summary (median timing per course + IQR + N)
course_set <- bioe_courses %>% arrange(Students) %>% pull(Course)

avg_course_pct_cs <- time_completed_summary %>%
  filter(SE.Subject.Catalog.Nbr %in% course_set) %>%
  group_by(SE.Subject.Catalog.Nbr) %>%
  summarise(
    median_pct = median(course_percentage_degree, na.rm = TRUE),
    lq         = quantile(course_percentage_degree, 0.25, na.rm = TRUE),
    uq         = quantile(course_percentage_degree, 0.75, na.rm = TRUE),
    n          = n(),
    Course     = first(SE.Subject.Catalog.Nbr),
    .groups    = "drop"
  ) %>%
  left_join(course_names, by = c("Course" = "Course")) %>%
  left_join(field_lu %>% select(SE.Subject.Catalog.Nbr, Intensity),
            by = c("Course" = "SE.Subject.Catalog.Nbr")) %>%
  mutate(
    Intensity = case_when(
      Course == "BIOE 195" ~ "Research",
      TRUE ~ as.character(Intensity)
    ),
    Intensity = factor(
      Intensity,
      levels = c("Introductory", "Low intensity", "Medium intensity", "Intensive", "Research")
    ),
    CourseLabel = coalesce(CourseLabel, Course),
    CourseLabel = fct_reorder(CourseLabel, median_pct)
  )

# Shared palette from your field-course plot
intensity_palette <- c(
  "Introductory"     = "#ffe0e9",
  "Low intensity"    = "#ff7aa2",
  "Medium intensity" = "#cc718d",
  "Intensive"        = "#602437",
  "Research"         = "#0a9396"
)

# Plot: median % of degree completed per course
timing_per_course_p <- ggplot(avg_course_pct_cs,
                              aes(x = CourseLabel, y = median_pct,
                                  fill = Intensity, size = n)) +
  geom_errorbar(aes(ymin = lq, ymax = uq),
                width = 0.12, linewidth = 0.3, color = "black") +
  geom_point(shape = 21, stroke = 0.6, color = "black", alpha = 0.9) +
  scale_fill_manual(values = intensity_palette, guide = "none") +   # color by intensity, hide legend
  scale_size_continuous(name = "# of Students", range = c(3, 10)) +  # keep only this legend
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(x = "Course", y = "% of Degree Completed") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",           # only size legend on the right
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.title = element_text(size = 20),
    panel.grid.minor = element_blank()
  )

timing_per_course_p
ggsave( filename = "./drafts/figures/timing_per_course_p.png", plot = timing_per_course_p, width = 12, height = 7, dpi = 600, bg = "white")

# Field-course count distribution per student (0 counts included).
# Counts distinct courses (excluding BIOE 195) drawn from your course_set.
full_counts <- bioe_students %>%
  distinct(CodedID) %>%
  left_join(
    bioe_students %>%
      filter(SE.Subject.Catalog.Nbr %in% setdiff(course_set, "BIOE 195")) %>%
      group_by(CodedID) %>%
      summarise(n_field_courses = n_distinct(SE.Subject.Catalog.Nbr), .groups = "drop"),
    by = "CodedID"
  ) %>%
  mutate(n_field_courses = replace_na(n_field_courses, 0L))

field_dist_cs <- full_counts %>%
  count(n_field_courses, name = "n_students") %>%
  mutate(prop = n_students / sum(n_students), pct = scales::percent(prop))

# Simple palette and mapping for the count bars
field_palette_5 <- c("#540b0e","#bb3e03","#ca6702","#ee9b00","#fff3b0")
count_levels <- sort(unique(field_dist_cs$n_field_courses))
field_cols   <- set_names(rep_len(field_palette_5, length(count_levels)), count_levels)

field_dist_cs <- field_dist_cs %>%
  mutate(
    pct_students = 100 * n_students / sum(n_students)  # convert to percentage
  )

num_field_courses_p <- ggplot(
  field_dist_cs,
  aes(x = factor(n_field_courses), y = pct_students, fill = factor(n_field_courses))
) +
  geom_col(color = "grey70", linewidth = 0.3) +
  scale_fill_manual(values = field_cols, guide = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = scales::percent_format(scale = 1)  # formats y-axis as %
  ) +
  labs(x = "Number of Field Courses", y = "Percentage of Students") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16)
  )

num_field_courses_p

ggsave(filename = "./drafts/figures/num_field_courses_dist.png", plot = num_field_courses_p, width = 10, height = 5, dpi = 600, bg = "white")

## =========================
## PRIOR-COURSE INDICATOR & GLMM
## =========================
# Goal: did students who completed 195 previously take ANY course with the same instructor?
# We match each 195 attempt to the earliest prior non-195 course with the same instructor.

# (1) Student-level outcome: ever took BIOE 195 (0/1)
student_summary1 <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%
  group_by(CodedID) %>%
  summarise(completed_BIOE_195 = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 195")),
            .groups = "drop")

# (2) BIOE 195 attempts with instructor + term
bioe195_inst <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  mutate(
    CodedID              = as.character(CodedID),
    bioe195_instructor   = str_squish(as.character(CI.Instructor.Name)),
    TA.Term.Code_bioe195 = suppressWarnings(as.integer(TA.Term.Code))
  ) %>%
  distinct(CodedID, bioe195_instructor, TA.Term.Code_bioe195)

# (3) Prior (non-195) enrollments for those students
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

# (4) For each 195 attempt, keep the earliest prior course with the SAME instructor
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

# (5) Student-level: flag whether any prior same-instructor course exists
student_match_full <- student_summary1 %>%
  left_join(prior_match_details, by = "CodedID") %>%
  mutate(had_prior_course = !is.na(prior_instructor))

# Quick sanity summary (among thesis completers)
student_match_full %>%
  filter(completed_BIOE_195 == 1) %>%
  summarise(
    total_thesis   = n(),
    n_with_prior   = sum(had_prior_course, na.rm = TRUE),
    pct_with_prior = mean(had_prior_course, na.rm = TRUE) * 100
  )

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
  filter(SE.Student.Enrollment.Status.Code == "E") %>%
  group_by(CodedID) %>%
  summarise(
    # outcome
    completed_BIOE_195  = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 195")),
    
    # ever-completed flags (selected field courses)
    completed_BIOE_82   = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 82")),
    completed_BIOE_128L = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 128L")),
    completed_BIOE_141L = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 141L")),
    completed_BIOE_151A = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 151A")),
    completed_BIOE_155L = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 155L")),
    
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
    .groups = "drop"
  )

gpa_latest <- time_completed_summary %>%
  transmute(
    CodedID = as.character(CodedID),
    term_i  = get_term_index(TA.Term.Code),
    gpa_i   = suppressWarnings(as.numeric(as.character(`TA.GPA...Cumulative`)))
  ) %>%
  filter(!is.na(CodedID), !is.na(term_i), !is.na(gpa_i)) %>%
  group_by(CodedID) %>%
  slice_max(term_i, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(CodedID, gpa_raw = gpa_i)

# 2) Admit year from admit term code (preferred)
admit_year_tbl <- bioe_students %>%
  transmute(
    CodedID = as.character(CodedID),
    year = get_year(UAD.Admit.Term.Code)  # your helper that parses term code → calendar year
  ) %>%
  distinct(CodedID, .keep_all = TRUE)

# Field-course summary per student: distinct #courses, latest field term, admit year, GPA
field_summary <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr %in% field_courses) %>%
  mutate(
    CodedID = as.character(CodedID),
    term_i  = get_term_index(TA.Term.Code)
  ) %>%
  group_by(CodedID) %>%
  summarise(
    n_field_courses = n_distinct(SE.Subject.Catalog.Nbr),
    term_code       = if (all(is.na(term_i))) NA_integer_ else max(term_i, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(time_completed_summary %>% distinct(CodedID = as.character(CodedID)), by = "CodedID") %>%
  mutate(n_field_courses = replace_na(n_field_courses, 0L)) %>%
  left_join(admit_year_tbl, by = "CodedID") %>%   # -> 'year'
  left_join(gpa_latest,     by = "CodedID")

# Demographics (latest record per student)
demo_vars <- bioe_students %>%
  mutate(CodedID = as.character(CodedID),
         term    = suppressWarnings(as.integer(TA.Term.Code))) %>%
  arrange(CodedID, desc(term)) %>%
  group_by(CodedID) %>%
  slice(1) %>%
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
    UAD.Applicant.GPA
  )

# Assemble master student-level frame
model_students <- student_summary %>%
  left_join(field_summary,      by = "CodedID") %>%
  left_join(student_match_full, by = "CodedID") %>%
  left_join(demo_vars,          by = "CodedID") %>%
  rename_with(~ sub("\\.y$", "", .), ends_with(".y")) %>%
  dplyr::select(-ends_with(".x")) %>%
  mutate(
    avg_time_pct_field   = rowMeans(across(starts_with("time_pct_")), na.rm = TRUE),
    avg_time_pct_field_s = as.numeric(scale(avg_time_pct_field)),
    completed_BIOE_195   = factor(completed_BIOE_195, levels = c(0, 1))
  ) %>%
  filter(!is.na(completed_BIOE_195)) %>%
  distinct(CodedID, .keep_all = TRUE)

# Clean levels & scale GPA; set reference levels for categorical predictors
model_students2 <- model_students %>%
  mutate(
    UAD.Applicant.GPA = na_if(UAD.Applicant.GPA, 0)
  )   %>%
  mutate(
    GPA_scaled = as.numeric(scale(gpa_raw)),
    app_GPA_scaled = as.numeric(scale(UAD.Applicant.GPA)),
    had_prior_course    = factor(had_prior_course, levels = c(FALSE, TRUE)),
    total_field_hours   = suppressWarnings(as.numeric(TotalFieldHours)),
    independent.project = factor(Independent.projects, levels = c("Yes", "No")),
    UAD.Admit.Type.Descr     = fct_relevel(UAD.Admit.Type.Descr, "First-Time Freshman"),
    PD.Gender...Current      = fct_relevel(PD.Gender...Current,  "F"),
    SGC.Is.EOP               = fct_relevel(SGC.Is.EOP,           "N"),
    ET.Ethnic.Group.Category = fct_relevel(ET.Ethnic.Group.Category, "White/Caucasian"),
    UAD.First.Generation     = fct_relevel(UAD.First.Generation, "Not First Generation")
  ) %>%
  distinct(CodedID, .keep_all = TRUE) 
# Minimal modeling frame
vars_used <- c(
  "completed_BIOE_195","n_field_courses","avg_time_pct_field_s",
  "UAD.Admit.Type.Descr","PD.Gender...Current","SGC.Is.EOP",
  "year","ET.Ethnic.Group.Category","UAD.First.Generation",
  "GPA_scaled","CodedID","total_field_hours", "app_GPA_scaled"
)

model_df <- model_students2 %>%
  dplyr::select(all_of(vars_used)) %>%
  mutate(completed_BIOE_195 = factor(completed_BIOE_195, levels = c(0, 1))) %>%
  droplevels() %>%
  filter(!is.na(UAD.Admit.Type.Descr)) %>%
  mutate(
    Status_simple = factor(
      ifelse(UAD.Admit.Type.Descr %in% c("Junior Transfer","Sophomore Transfer","Senior Transfer"),
             "Transfer", "First-Time Freshman"),
      levels = c("First-Time Freshman","Transfer")
    ),
    year_s = as.numeric(scale(year))
  ) %>% 
  filter(!ET.Ethnic.Group.Category %in% c(" -", "Unknown/Not Specified"),
         !UAD.First.Generation %in% "Unknown/Other")

# Logistic model on minimal frame
glm_student2_clean <- glm(
  completed_BIOE_195 ~ factor(n_field_courses) +
    Status_simple + PD.Gender...Current + SGC.Is.EOP +
    ET.Ethnic.Group.Category + UAD.First.Generation + GPA_scaled + app_GPA_scaled,
  data = model_df,
  family = binomial("logit")
)
summary(glm_student2_clean)
r.squaredGLMM(glm_student2_clean)
cor.test(model_df$n_field_courses, model_df$avg_time_pct_field_s)
#robust_ct(glm_student2_clean,   model_df, fallback = "year") #ICC close to 0, do not compute robust transform

# Quick visuals
gpa_palette <- c("0" = "#0a9396", "1" = "#94d2bd")

m_num_field_courses <- ggplot(
  model_df,
  aes(
    x = factor(completed_BIOE_195, levels = c(0, 1), labels = c("No Thesis", "Thesis Completed")),
    fill = factor(n_field_courses)
  )
) +
  geom_bar(position = "fill", color = "white") +
  scale_fill_manual(values = field_palette_5) +
  labs(
    x = "Completed Thesis",
    y = "Proportion of Students",
    fill = "Field Course Count"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid   = element_blank(),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 20),
    axis.text    = element_text(size = 16),
    axis.title   = element_text(size = 25)
  ); m_num_field_courses

ggsave( filename = "./drafts/figures/m_num_field_courses.png", plot = m_num_field_courses, width = 8, height = 10.5, dpi = 600, bg = "white")

p_gpa <- ggplot(model_df,
                aes(x = factor(completed_BIOE_195),
                    y = GPA_scaled,
                    fill = factor(completed_BIOE_195))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "grey20") +
  geom_jitter(width = 0.22, size = 1.4, alpha = 0.3, color = "grey30") +
  scale_fill_manual(values = gpa_palette, labels = c("0 = No","1 = Yes")) +
  labs(x = "Senior Thesis Completion", y = "Scaled GPA", fill = "Thesis Completion") +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank()); p_gpa

# ORs (human-friendly labels)
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

car::vif(glm_student2_clean)


## =========================
## PER-COURSE TIMING GLM
## (Took + Timing per course, plus GPA)
## =========================

## -------------------------------
## 0) Course set to analyze
## -------------------------------
all_courses <- c("BIOE 82","BIOE 128L","BIOE 141L","BIOE 155L","BIOE 151A")

## -------------------------------
## 1) Student-level thesis outcome (response: ever completed BIOE 195)
## -------------------------------
stu_outcome <- time_completed_summary %>%
  mutate(CodedID = as.character(CodedID)) %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%
  group_by(CodedID) %>%
  summarise(
    completed_BIOE_195 = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 195")),
    .groups = "drop"
  )

## -------------------------------
## 2) BIOE 195 attempt info (for “prior same-instructor” checks)
## -------------------------------
bioe195_inst <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  mutate(
    CodedID              = as.character(CodedID),
    bioe195_instructor   = stringr::str_squish(as.character(CI.Instructor.Name)),
    TA.Term.Code_bioe195 = suppressWarnings(as.integer(TA.Term.Code))
  ) %>%
  distinct(CodedID, bioe195_instructor, TA.Term.Code_bioe195)

## -------------------------------
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
    course != "BIOE 195"
  ) %>%
  # keep the admission year that was already computed upstream in time_completed_summary
  distinct(CodedID, course, term, instructor, year, .keep_all = TRUE) %>%
  dplyr::select(CodedID, course, term, instructor, course_percentage_degree, year)

## -------------------------------
## 4) Course attributes (join)
## -------------------------------
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

enroll_field <- enroll_field %>% left_join(course_info, by = "course")

## -------------------------------
## 5) Student-level summaries from non-195 enrollments
## -------------------------------
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

## -------------------------------
## 6) Prior same-instructor flag (non-195 → 195)
## -------------------------------
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

## -------------------------------
## 7) Final student-level modeling table
## -------------------------------
student_level <- stu_outcome %>%
  left_join(student_field_profile,  by = "CodedID") %>%
  left_join(prior_same_instructor,  by = "CodedID") %>%
  mutate(
    avg_time_pct_field_s = as.numeric(scale(avg_time_pct_field, center = TRUE, scale = TRUE)),
    TotalFieldHours_s    = as.numeric(scale(TotalFieldHours, center = TRUE, scale = TRUE)),
    TotalUnits_s         = as.numeric(scale(TotalUnits, center = TRUE, scale = TRUE)),
    prior_same_inst      = factor(replace_na(prior_same_inst, FALSE), levels = c(FALSE, TRUE)),
    year                 = as.integer(year)  # keep numeric for now; we’ll factor it for ICC
  ) %>%
  filter(!is.na(n_field_courses) & n_field_courses > 0) %>%
  distinct(CodedID, .keep_all = TRUE)

## ---------------------------
## I) Logistic model (as before)
## ---------------------------
m_student_glm <- glm(
  completed_BIOE_195 ~
    avg_time_pct_field_s + TotalUnits + indep_proj_any + Intensity,
  data   = student_level,
  family = binomial("logit")
)

summary(m_student_glm)
MuMIn::r.squaredGLMM(m_student_glm)
car::vif(m_student_glm)

## ---------------------------
## ICC for YEAR (null random-intercept model)
## ---------------------------
student_level <- student_level %>%
  mutate(
    year_f   = factor(year))

## Null random-intercept GLMM by YEAR (reference)
m0_year <- lme4::glmer(
  completed_BIOE_195 ~ 1 + (1 | year_f),
  data   = student_level,
  family = binomial
)

var_year <- as.numeric(lme4::VarCorr(m0_year)$year_f[1])
icc_year <- var_year / (var_year + (pi^2 / 3))
cat(sprintf("ICC (year): %.4f; groups=%d\n",
            icc_year, nlevels(student_level$year_f)))

## ---------------------------
## Diagnostics (+ dispersion/zeros)
## ---------------------------
sim <- DHARMa::simulateResiduals(m_student_glm, n = 1000)
plot(sim)

## ---------------------------
## Simple visuals (sanity checks)
## ---------------------------

# Palette reused elsewhere
gpa_palette <- c("Thesis Completed" = "#0a9396", "No Thesis" = "#94d2bd")

# Box/jitter: average timing vs completion
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
    x = "Senior Thesis Completion",
    y = "Average timing of field courses (% of degree)",
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
  )

p_time

ggsave( filename = "./drafts/figures/box_avg_timing.png", plot = p_time, width = 12, height = 7, dpi = 600, bg = "white")

## ========================
## FIELD-COURSE MODELS + PER-COURSE CURVES
## ========================
# Build first-year columns for each course and join back once.
# (We reuse get_year() already defined; no need for another to_year().)
all_courses <- c("BIOE 82","BIOE 128L","BIOE 141L","BIOE 151A","BIOE 155L")

course_years_wide <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr %in% all_courses) %>%
  transmute(
    CodedID = as.character(CodedID),
    course  = gsub(" ", "_", SE.Subject.Catalog.Nbr),    # BIOE_82
    year_fc = get_year(TA.Term.Code)
  ) %>%
  group_by(CodedID, course) %>%
  summarise(first_year = suppressWarnings(min(year_fc, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = course, values_from = first_year, names_prefix = "year_")

model_students2 <- model_students2 %>%
  mutate(CodedID = as.character(CodedID)) %>%
  # add per-course first-year columns (already built in course_years_wide)
  left_join(course_years_wide, by = "CodedID") %>%
  mutate(across(starts_with("year_"), ~ factor(.x))) %>%
  # add primary instructor from your student_field_profile
  left_join(
    student_field_profile %>%
      dplyr::distinct(CodedID, instructor_primary),
    by = "CodedID"
  )

# Per-course subsets for GLMs (keep rows that actually took the course → timing not NA)
model_students_82_only   <- model_students2 %>% filter(!is.na(time_pct_BIOE_82))
model_students_128L_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_128L))
model_students_141L_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_141L))
model_students_151A_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_151A))
model_students_155L_only <- model_students2 %>% filter(!is.na(time_pct_BIOE_155L))

# Fit per-course GLMs (probability of thesis ~ timing + GPA)
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

# Quick summaries + residual checks
summary(glm_BIOE_82);   plot(simulateResiduals(glm_BIOE_82,   n = 1000))
summary(glm_BIOE_128L); plot(simulateResiduals(glm_BIOE_128L, n = 1000))
summary(glm_BIOE_141L); plot(simulateResiduals(glm_BIOE_141L, n = 1000))
summary(glm_BIOE_151A); plot(simulateResiduals(glm_BIOE_151A, n = 1000))
summary(glm_BIOE_155L); plot(simulateResiduals(glm_BIOE_155L, n = 1000))

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
robust_ct <- function(model,
                      data,
                      candidates = c("instructor_primary", "year"),
                      icc_threshold = 0.01,
                      min_levels = 2) {
  # Rows actually used by the model
  mf  <- model.frame(model)
  idx <- rownames(mf)
  
  # Align 'data' to model rows if possible; else use mf
  dat_aligned <- tryCatch(data[idx, , drop = TRUE], error = function(e) NULL)
  if (is.null(dat_aligned)) dat_aligned <- mf
  
  # Only consider instructor_primary / year if present
  cand_avail <- intersect(candidates, union(names(dat_aligned), names(mf)))
  
  # Build ICC table for the two candidates
  icc_rows <- lapply(cand_avail, function(cl_nm) {
    cl_vec <- if (cl_nm %in% names(dat_aligned)) dat_aligned[[cl_nm]] else mf[[cl_nm]]
    if (is.null(cl_vec) || length(cl_vec) != nrow(mf)) return(NULL)
    levs <- nlevels(factor(cl_vec))
    icc  <- .icc_glmm_binom(data.frame(yy = mf[[1]], cl = cl_vec), "yy", "cl")
    tibble::tibble(cluster = cl_nm, levels = levs, icc = icc)
  })
  icc_table <- dplyr::bind_rows(icc_rows)
  
  # Pick cluster with non-trivial ICC
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
    cluster_used <- "(row)"                           # HC1, no clustering
    cl <- factor(seq_len(nrow(mf)))
    if (nrow(icc_table) > 0) {
      message(sprintf("ICC check: no candidate ICC ≥ %.4f (using row-level HC1).", icc_threshold))
    } else {
      message("ICC check: no candidate variables available in model rows (using row-level HC1).")
    }
  }
  
  # Robust vcov + tests
  Vc <- sandwich::vcovCL(model, cluster = cl, type = "HC1")
  ct <- lmtest::coeftest(model, vcov. = Vc)
  
  list(
    coeftest     = ct,
    vcov         = Vc,
    cluster_used = cluster_used,
    icc_table    = icc_table
  )
}

res_82   <- robust_ct(glm_BIOE_82,   model_students_82_only) #Use robust_ct for results
res_128L <- robust_ct(glm_BIOE_128L, model_students_128L_only) #Use robust_ct for results
res_141L <- robust_ct(glm_BIOE_141L, model_students_141L_only) #Use regular summary
res_151A <- robust_ct(glm_BIOE_151A, model_students_151A_only) #Use robust_ct for results
res_155L <- robust_ct(glm_BIOE_155L, model_students_155L_only) #Use robust_ct for results 

res_82$icc_table; res_82$coeftest; res_82$cluster_used
res_128L$icc_table; res_128L$coeftest; res_128L$cluster_used
res_151A$icc_table; res_151A$coeftest; res_151A$cluster_used
res_155L$icc_table; res_155L$coeftest; res_155L$cluster_used


## =========================
## SIG TIMING: PROB TABLE + PLOT (ROBUST, MINIMAL)
## =========================
alpha <- 0.05

# pretty labels per course/term
candidates <- list(
  "BIOE 82"   = c("time_pct_BIOE_82"   = "BIOE 82 — Timing"),   # timing is the sig one
  "BIOE 128L" = c("app_GPA_scaled"     = "BIOE 128L — Applicant GPA",
                  "time_pct_BIOE_128L" = "BIOE 128L — Timing"),
  "BIOE 141L" = c("time_pct_BIOE_141L" = "BIOE 141L — Timing"),
  "BIOE 151A" = c("time_pct_BIOE_151A" = "BIOE 151A — Timing",
                  "app_GPA_scaled"     = "BIOE 151A — Applicant GPA"),
  "BIOE 155L" = c("time_pct_BIOE_155L" = "BIOE 155L — Timing")
)

line_cols <- c(
  "BIOE 82 — Timing"              = "#0065aaff",
  "BIOE 128L — Applicant GPA"     = "#ff9354ff",
  "BIOE 141L — Timing"            = "#f6bf3eff",
  "BIOE 151A — Timing"            = "#f6bf3eff",
  "BIOE 151A — Applicant GPA"     = "#ffb665ff",
  "BIOE 155L — Timing"            = "#bb3a8dff"
)
.lighten <- function(col, f = 0.55) {
  m <- grDevices::col2rgb(col); m2 <- m * (1 - f) + 255 * f
  grDevices::rgb(m2[1,], m2[2,], m2[3,], maxColorValue = 255)
}
ribbon_cols <- vapply(line_cols, .lighten, character(1))

# ---- use the exact cluster from res_* (aligned to model rows)
.get_cluster_from_res <- function(model, data, res) {
  mf <- model.frame(model)
  dat_aligned <- tryCatch(data[rownames(mf), , drop = FALSE], error = function(e) mf)
  cu <- res$cluster_used
  if (is.null(cu) || identical(cu, "(row)") || !cu %in% names(dat_aligned)) {
    return(factor(seq_len(nrow(mf))))
  }
  factor(dat_aligned[[cu]])
}

# ---- table of p-values for cluster-robust models
.table_from_res <- function(model, data, res) {
  cl <- .get_cluster_from_res(model, data, res)
  Vc <- sandwich::vcovCL(model, cluster = cl, type = "HC1")
  beta <- coef(model); se <- sqrt(diag(Vc))
  tibble(term = names(beta),
         p = 2 * pnorm(abs(beta / se), lower.tail = FALSE))
}

# ---- safe curve builders (std for 141L, res for others)
.make_grid <- function(model, term, n = 200) {
  mf <- model.frame(model)
  xvec <- mf[[term]]
  rng <- range(xvec[is.finite(xvec)], na.rm = TRUE)
  if (!all(is.finite(rng))) return(NULL)
  new <- tibble(!!rlang::sym(term) := seq(rng[1], rng[2], length.out = n))
  others <- setdiff(attr(stats::terms(model), "term.labels"), term)
  for (t in others) {
    if (!t %in% names(mf)) next
    if (is.numeric(mf[[t]]) || is.integer(mf[[t]])) {
      new[[t]] <- median(mf[[t]], na.rm = TRUE)
    } else {
      new[[t]] <- factor(levels(mf[[t]])[1], levels = levels(mf[[t]]))
    }
  }
  new
}
.make_X <- function(model, newdata) {
  TT <- stats::delete.response(stats::terms(model))
  X  <- model.matrix(TT, newdata, contrasts.arg = model$contrasts, xlev = model$xlevels)
  X[, names(coef(model)), drop = FALSE]
}
.curve_std <- function(model, term, label, n = 200) {
  new <- .make_grid(model, term, n); if (is.null(new)) return(NULL)
  X   <- .make_X(model, new)
  beta <- coef(model); V <- vcov(model)
  link <- as.numeric(X %*% beta)
  se   <- sqrt(rowSums((X %*% V) * X))
  tibble(panel = label, x = new[[term]],
         prob = plogis(link), lo = plogis(link - 1.96*se), hi = plogis(link + 1.96*se))
}
.curve_res <- function(model, data, res, term, label, n = 200) {
  new <- .make_grid(model, term, n); if (is.null(new)) return(NULL)
  X   <- .make_X(model, new)
  cl  <- .get_cluster_from_res(model, data, res)
  Vc  <- sandwich::vcovCL(model, cluster = cl, type = "HC1")
  beta <- coef(model)
  link <- as.numeric(X %*% beta)
  se   <- sqrt(rowSums((X %*% Vc) * X))
  tibble(panel = label, x = new[[term]],
         prob = plogis(link), lo = plogis(link - 1.96*se), hi = plogis(link + 1.96*se))
}

# ---- collect significant terms (141L uses summary, others use res)
sig_list <- bind_rows(
  { # 141L via summary()
    sm <- summary(glm_BIOE_141L)$coefficients
    tb <- tibble(term = rownames(sm), p = sm[, ncol(sm)])
    cand <- names(candidates[["BIOE 141L"]])
    tb %>% filter(term %in% cand, is.finite(p), p < alpha) %>%
      mutate(panel = unname(candidates[["BIOE 141L"]][term]),
             course = "BIOE 141L")
  },
  { # 82 via res
    tb <- .table_from_res(glm_BIOE_82, model_students_82_only, res_82)
    cand <- names(candidates[["BIOE 82"]])
    tb %>% filter(term %in% cand, is.finite(p), p < alpha) %>%
      mutate(panel = unname(candidates[["BIOE 82"]][term]),
             course = "BIOE 82")
  },
  { # 128L
    tb <- .table_from_res(glm_BIOE_128L, model_students_128L_only, res_128L)
    cand <- names(candidates[["BIOE 128L"]])
    tb %>% filter(term %in% cand, is.finite(p), p < alpha) %>%
      mutate(panel = unname(candidates[["BIOE 128L"]][term]),
             course = "BIOE 128L")
  },
  { # 151A
    tb <- .table_from_res(glm_BIOE_151A, model_students_151A_only, res_151A)
    cand <- names(candidates[["BIOE 151A"]])
    tb %>% filter(term %in% cand, is.finite(p), p < alpha) %>%
      mutate(panel = unname(candidates[["BIOE 151A"]][term]),
             course = "BIOE 151A")
  },
  { # 155L
    tb <- .table_from_res(glm_BIOE_155L, model_students_155L_only, res_155L)
    cand <- names(candidates[["BIOE 155L"]])
    tb %>% filter(term %in% cand, is.finite(p), p < alpha) %>%
      mutate(panel = unname(candidates[["BIOE 155L"]][term]),
             course = "BIOE 155L")
  }
)

# ---- build curves
curves_sig <- sig_list %>%
  dplyr::select(course, term, panel) %>%
  purrr::pmap_dfr(function(course, term, panel) {
    if (course == "BIOE 141L") {
      .curve_std(glm_BIOE_141L, term, panel)
    } else if (course == "BIOE 82") {
      .curve_res(glm_BIOE_82, model_students_82_only, res_82, term, panel)
    } else if (course == "BIOE 128L") {
      .curve_res(glm_BIOE_128L, model_students_128L_only, res_128L, term, panel)
    } else if (course == "BIOE 151A") {
      .curve_res(glm_BIOE_151A, model_students_151A_only, res_151A, term, panel)
    } else if (course == "BIOE 155L") {
      .curve_res(glm_BIOE_155L, model_students_155L_only, res_155L, term, panel)
    } else {
      tibble::tibble()  # safe empty tibble if some other course sneaks in
    }
  })

# ---- plot
panel_order <- c("BIOE 82 — Timing",
                 "BIOE 128L — Applicant GPA",
                 "BIOE 141L — Timing",
                 "BIOE 151A — Timing",
                 "BIOE 151A — Applicant GPA",
                 "BIOE 155L — Timing")
curves_sig$panel <- factor(curves_sig$panel, levels = panel_order)

line_cols_used   <- line_cols[names(line_cols) %in% levels(curves_sig$panel)]
ribbon_cols_used <- vapply(line_cols_used, .lighten, character(1))

p_sig_curves <- ggplot(curves_sig, aes(x = x, y = prob, color = panel, fill = panel)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.35, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ panel, ncol = 1, scales = "free_x") +
  scale_color_manual(values = line_cols_used, guide = "none") +
  scale_fill_manual(values = ribbon_cols_used, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = NULL, y = "Predicted Probability of Thesis Completion") +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size = 13),  
    axis.title.x = element_text(margin = margin(t = 6), size = 20),
    axis.title.y = element_text(margin = margin(r = 6), size = 20),
    axis.text = element_text(size = 15),
    panel.spacing = unit(1.5, "lines")   # ⬅️ increase spacing between facet panels
  )

print(p_sig_curves)

ggsave(filename = "./drafts/figures/prob_curves_per_course.png", plot = p_sig_curves, width = 8, height = 10, dpi = 600, bg = "white")
