# validation_checks.R
# Mock CDM study (based on public trial NCT01719003) - DUMMY DATA ONLY
#
# What this script does:
#   1. Reads every form (sheet) of the dummy dataset into R
#   2. Runs data checks (each check is a RULE that good data must obey)
#   3. Collects every rule-breaking row in one table: the DISCREPANCY LISTING
#   4. Saves that table as a CSV (this becomes your query log later)
#
# How to run: put the cursor on a line and press Ctrl+Enter (Cmd+Enter on Mac)
# to run one line. Or press the "Source" button to run the whole file.
# Tip: run the script step by step and look at each result before moving on.

#-----
  

# ---------------------------------------------------------------------
# PART 1: Load the tools (packages)
# ---------------------------------------------------------------------
# A package is a toolbox someone else wrote. library() opens it.

# install.packages(c("readxl","dplyr","tidyr","purrr","writexl"))
library(readxl); library(dplyr); library(tidyr); library(purrr); library(writexl)

# reads Excel files
# filters, joins and summarises tables

# ---------------------------------------------------------------------
# PART 2: Read the data
# ---------------------------------------------------------------------
# "f" is just a name that holds the path to the Excel file.
# "<-" means "store the thing on the right into the name on the left".
f <- "03_Data/Dummy_Data_30_Subjects.xlsx"


# read_excel() reads ONE sheet of the file. Each sheet is one CRF form.
# Each line below creates a table (R calls it a data frame) named ie, dm, ...
ie <- read_excel(f, sheet = "consent_eligibility")        # consent + eligibility
dm <- read_excel(f, sheet = "demographics")               # age, sex, race
rd <- read_excel(f, sheet = "randomization")              # randomisation number/date
vs <- read_excel(f, sheet = "vital_signs")                # BP, heart rate, weight
lb <- read_excel(f, sheet = "laboratory")                 # HbA1c, glucose, creatinine
ex <- read_excel(f, sheet = "study_drug_exposure")        # drug kits, tablets
ae <- read_excel(f, sheet = "adverse_events")             # adverse events
cm <- read_excel(f, sheet = "concomitant_medications")    # other medicines
ds <- read_excel(f, sheet = "disposition")                # completed / discontinued

# Check your work: after running the lines above, the Environment pane
# (top right) should list 9 tables with these numbers of rows:
#   ie 60, dm 31, rd 30, vs 115, lb 145, ex 85, ae 37, cm 58, ds 30



# ---------------------------------------------------------------------
# PART 3: Make an empty discrepancy listing and a helper function
# ---------------------------------------------------------------------
# tibble() makes an empty table with 6 columns that we will fill up.
# character() means "text column".
findings <- tibble(subject_id = character(),   # which subject
                   form       = character(),    # which CRF form
                   visit      = character(),    # which visit (event)
                   variable   = character(),    # which field
                   value      = character(),    # the bad value
                   check      = character())    # which rule it broke

# A function is a mini-program you can reuse. This one is called add_findings.
# Every check hands it the bad rows and it adds them to the listing, so we
# don't repeat the same 5 lines for every check.
#   df       = the table of rows that broke the rule
#   form     = text: the name of the form (for the listing)
#   variable = text: the column name the problem is in
#   check    = text: the rule's description (for the listing)
#   value    = what to show as the bad value (default: the column "variable")
add_findings <- function(df, form, variable, check, value = df[[variable]]) {
  
  # If no rows broke the rule, there is nothing to add: stop quietly.
  if (nrow(df) == 0) return(invisible(NULL))
  
  # Build a small table with one row per bad record.
  new <- tibble(subject_id = df$subject_id,
                form       = form,
                visit      = df$redcap_event_name,
                variable   = variable,
                value      = as.character(value),
                check      = check)
  
  # "<<-" updates the 'findings' table that lives OUTSIDE the function.
  # bind_rows() stacks the new rows underneath the old ones.
  findings <<- bind_rows(findings, new)
}

# ---------------------------------------------------------------------
# PART 4: The checks
# Pattern for EVERY check:
#   (a) x <- table %>% filter(<condition that is WRONG>)
#   (b) add_findings(x, form, variable, description)
# "%>%" is the pipe: read it as "and then".
# filter() keeps only the rows where the condition is TRUE.
# ---------------------------------------------------------------------

# --- Check 1: RANGE CHECK -------------------------------------------
# Rule: systolic BP must be between 70 and 250 mmHg.
# "|" means OR. So: keep rows where sysbp is below 70 OR above 250.
# (Rows where sysbp is blank are not caught here; that is a separate check.)
x <- vs %>% filter(sysbp < 70 | sysbp > 250)
add_findings(x, "vital_signs", "sysbp", "Systolic BP outside 70-250")

# --- Check 2: LOGIC CHECK (two fields compared) ---------------------
# Rule: systolic must be higher than diastolic.
# We keep rows where systolic is lower than or equal to diastolic (wrong).
x <- vs %>% filter(sysbp <= diabp)
# For the "value" we show both numbers, like 78/92.
# paste() joins pieces of text; sep = "/" is the glue between them.
add_findings(x, "vital_signs", "sysbp", "Systolic not greater than diastolic",
             value = paste(x$sysbp, x$diabp, sep = "/"))

# --- Check 3: MISSING DATA ------------------------------------------
# Rule: HbA1c (the primary outcome) must never be blank.
# is.na() is TRUE when a cell is empty. In R a blank is called NA.
x <- lb %>% filter(is.na(hba1c))
add_findings(x, "laboratory", "hba1c", "HbA1c missing")

# --- Check 4: MISSING DATA ------------------------------------------
# Rule: sex must be recorded.
x <- dm %>% filter(is.na(sex))
add_findings(x, "demographics", "sex", "Sex missing")

# --- Check 5: DUPLICATE RECORD --------------------------------------
# Rule: each subject has exactly ONE demographics record.
# group_by(subject_id)  -> treat each subject as a group
# filter(n() > 1)       -> keep groups with more than 1 row (n() counts rows)
# slice(1)              -> keep just the first row of each group, so the
#                          duplicate is listed once, not twice
# ungroup()             -> stop treating the table as groups
x <- dm %>% group_by(subject_id) %>% filter(n() > 1) %>% slice(1) %>% ungroup()
add_findings(x, "demographics", "subject_id", "Duplicate demographics record")

consent <- ie %>%
  filter(redcap_event_name == "screening_arm_1") %>%
  select(subject_id, icdat)
# Check 6


x <- lb %>%
  left_join(consent, by = "subject_id") %>%
  filter(lbdat < icdat)
add_findings(x, "laboratory", "lbdat", "Sample collected before consent",
             value = paste0("sample ", as.Date(x$lbdat), " / consent ", as.Date(x$icdat)))

# Check 7
x <- ae %>% filter(aeendat < aestdat)
add_findings(x, "adverse_events", "aeendat", "AE end date before start date",
             value = paste0("start ", as.Date(x$aestdat), " / end ", as.Date(x$aeendat)))

# Check 8: 
#diastolic BP must be 40-150
x <- vs %>% filter(diabp < 40 | diabp > 150)
add_findings(x, "vital_signs", "diabp", "Diastolic BP outside 40-150")


#check 9:
#heart rate must be 40-150
x <- vs %>% filter(hr < 40 | hr > 150)
add_findings(x, "vital_signs", "hr", "Heart rate outside 40-150")


#check 10 :
#HbA1c must be 7.5-12
x <- lb %>% filter(redcap_event_name == "screening_arm_1" & (hba1c < 7.5 | hba1c > 12))
add_findings(x, "laboratory", "hba1c", "Screening hba1c outside 7.5-12")

#check 11:
#BMI should above 45
x <- vs %>% filter(redcap_event_name == "screening_arm_1" & bmi > 45)
add_findings(x,"vital_signs", "bmi" ,"Screening bmi above 45")

# --- Check 13: glucose reported although the subject did not fast ---
# lbfast: 1 = Yes, 0 = No.  "!is.na(fpg)" means "fpg is NOT blank".
x <- lb %>% filter(lbfast == 0 & !is.na(fpg))
add_findings(x, "laboratory", "lbfast", "FPG reported but subject not fasting")

# --- Check 14: creatinine must be 0.3-5.0 mg/dL ---
x <- lb %>% filter(creat < 0.3 | creat > 5)
add_findings(x, "laboratory", "creat", "Creatinine outside 0.3-5.0")

# --- Check 15: subject must be 18 or older ---
x <- dm %>% filter(age < 18)
add_findings(x, "demographics", "age", "Age under 18 at consent")

# --- Check 16: visit date must not be in the future ---
# as.Date() drops the time part so we compare date with date.
# Sys.Date() is today's date from your computer's clock.
x <- vs %>% filter(as.Date(vsdat) > Sys.Date())
add_findings(x, "vital_signs", "vsdat", "Visit date in the future",
             value = as.character(as.Date(x$vsdat)))

# --- Check 17: tablets returned must not exceed tablets dispensed ---
# Rows where either value is blank are skipped automatically.
x <- ex %>% filter(exretn > exdispn)
add_findings(x, "study_drug_exposure", "exretn", "Tablets returned exceed dispensed",
             value = paste0("returned ", x$exretn, " / dispensed ", x$exdispn))

#check 17;
# Height must be recorded at screening
x <- vs %>% filter(redcap_event_name == "screening_arm_1" & is.na(height))
add_findings(x, "vital_signs", "height", "Height missing at screening")

# --- Check 18: run-in FPG above 240 needs a repeat value ---
x <- lb %>% filter(redcap_event_name == "runin_arm_1" & fpg > 240 & is.na(fpg2))
add_findings(x, "laboratory", "fpg", "Run-in FPG above 240 with no repeat value")

# --- Check 19: repeat FPG must be on a different day from the first ---
x <- lb %>% filter(!is.na(fpg2dat) & as.Date(fpg2dat) == as.Date(lbdat))
add_findings(x, "laboratory", "fpg2dat", "Repeat FPG on same day as first",
             value = paste0("first ", as.Date(x$lbdat), " / repeat ", as.Date(x$fpg2dat)))

# --- Check 20: an ongoing adverse event must not have an end date ---
# aeongo: 1 = Yes
x <- ae %>% filter(aeongo == 1 & !is.na(aeendat))
add_findings(x, "adverse_events", "aeongo", "AE marked ongoing but has an end date",
             value = paste0("end date ", as.Date(x$aeendat)))

# --- Check 21: a serious adverse event needs seriousness criteria ---
# aeser: 1 = Yes
x <- ae %>% filter(aeser == 1 & is.na(aesercrit))
add_findings(x, "adverse_events", "aesercrit", "Serious AE with no seriousness criteria")

# --- Check 22: a discontinued subject needs a reason ---
# dsdecod: 1 = Completed, 2 = Discontinued
x <- ds %>% filter(dsdecod == 2 & is.na(dsreas))
add_findings(x, "disposition", "dsreas", "Discontinued with no reason")

# --- Check 23: a medication cannot end before it starts ---
x <- cm %>% filter(cmendat < cmstdat)
add_findings(x, "concomitant_medications", "cmendat", "Medication end before start",
             value = paste0("start ", as.Date(x$cmstdat), " / end ", as.Date(x$cmendat)))

# --- Check 24: medication dose must be greater than 0 ---
x <- cm %>% filter(cmdose <= 0)
add_findings(x, "concomitant_medications", "cmdose", "Medication dose is zero or negative")

# --- Check 25: a dose needs a unit ---
x <- cm %>% filter(!is.na(cmdose) & is.na(cmdosu))
add_findings(x, "concomitant_medications", "cmdosu", "Dose recorded without a unit")


# --- check 26: 
x <- ae %>% filter(aeterm %in% c("unwell"))
add_findings(x, "adverse_events", "aeterm", "Vague AE term cannot be coded")

# --- Check 27: an adverse event cannot start before consent ---
# 'consent' is the lookup table you made for Check 6.
x <- ae %>% left_join(consent, by = "subject_id") %>% filter(aestdat < icdat)
add_findings(x, "adverse_events", "aestdat", "AE starts before consent",
             value = paste0("AE start ", as.Date(x$aestdat), " / consent ", as.Date(x$icdat)))

# --- Check 28: randomisation cannot be before the run-in visit ---
# Lookup: the run-in visit date for each subject (taken from the lab sheet).
runin <- lb %>% filter(redcap_event_name == "runin_arm_1") %>%
  select(subject_id, runin_date = lbdat)       # "runin_date = lbdat" renames the column
x <- rd %>% left_join(runin, by = "subject_id") %>% filter(randdat < runin_date)
add_findings(x, "randomization", "randdat", "Randomised before run-in visit",
             value = paste0("randomised ", as.Date(x$randdat), " / run-in ", as.Date(x$runin_date)))

# --- Check 29: the first dose cannot be before randomisation ---
rand_dates <- rd %>% select(subject_id, randdat)
x <- ex %>% filter(!is.na(exstdat)) %>%          # only rows that have a first-dose date
  left_join(rand_dates, by = "subject_id") %>%
  filter(exstdat < randdat)
add_findings(x, "study_drug_exposure", "exstdat", "First dose before randomisation",
             value = paste0("first dose ", as.Date(x$exstdat), " / randomised ", as.Date(x$randdat)))

# --- Check 30: investigator says "not eligible" but the subject was randomised ---
# semi_join keeps rows of the left table that HAVE a match in the right table.
x <- ie %>% filter(ieelig == 0) %>% semi_join(rd, by = "subject_id")
add_findings(x, "consent_eligibility", "ieelig", "Not eligible but randomised",
             value = "eligible = No")

# --- Check 31: the disposition date cannot be before the last dose ---
last_dose <- ex %>% group_by(subject_id) %>%
  summarise(last_dose = max(exendat, na.rm = TRUE))   # latest last-dose date per subject
x <- ds %>% left_join(last_dose, by = "subject_id") %>% filter(dsdat < last_dose)
add_findings(x, "disposition", "dsdat", "Disposition date before last dose",
             value = paste0("disposition ", as.Date(x$dsdat), " / last dose ", as.Date(x$last_dose)))

# --- Check 32: marked completed, but there is no Week 24 data ---
# anti_join keeps rows of the left table that have NO match in the right table.
wk24 <- lb %>% filter(redcap_event_name == "week24_arm_1") %>% select(subject_id)
x <- ds %>% filter(dsdecod == 1) %>% anti_join(wk24, by = "subject_id")
add_findings(x, "disposition", "dsdecod", "Completed but no Week 24 data",
             value = "Completed, no Week 24 lab record")

# --- Check 33: randomisation number must be unique ---
# group_by(randno) puts rows with the same number in one group.
# filter(n() > 1) keeps groups with 2+ rows. No slice(1) this time, so BOTH
# subjects sharing the number are listed (each one needs a query).
# Expected: 2 rows
x <- rd %>% group_by(randno) %>% filter(n() > 1) %>% ungroup()
add_findings(x, "randomization", "randno", "Duplicate randomisation number")

# --- Check 34: a drug kit number must belong to ONE subject ---
# Blank kit numbers (visits with no dispensing) are removed first, or all
# the blanks would count as "duplicates" of each other.
# Expected: 2 rows
x <- ex %>% filter(!is.na(exkitno)) %>%
  group_by(exkitno) %>% filter(n() > 1) %>% ungroup()
add_findings(x, "study_drug_exposure", "exkitno", "Drug kit number used by more than one subject")

# --- Check 35: weight changed by more than 10% since the previous visit ---
# arrange() puts each subject's visits in date order, so lag() can look back.
# lag(weight) = the weight from the previous row WITHIN the same subject
# (group_by makes sure it never borrows from another subject).
# Expected: 1 row
x <- vs %>%
  filter(!is.na(weight)) %>%
  arrange(subject_id, vsdat) %>%
  group_by(subject_id) %>%
  mutate(prev_weight = lag(weight),
         pct_change  = (weight - prev_weight) / prev_weight * 100) %>%
  ungroup() %>%
  filter(abs(pct_change) > 10)       # abs() so gains AND losses are caught
add_findings(x, "vital_signs", "weight", "Weight changed by more than 10% between visits",
             value = paste0(x$prev_weight, " -> ", x$weight, " kg (", round(x$pct_change, 1), "%)"))

# --- Check 36: screening eCCr below 60, but exclusion 3 answered "No" ---
# excl03 = 0 means "does not meet this exclusion", i.e. subject is allowed in.
# Two lookups on different forms, so we select() only what we need, then join.
ccr <- lb %>% filter(redcap_event_name == "screening_arm_1") %>% select(subject_id, eccr)
excl <- ie %>% filter(redcap_event_name == "screening_arm_1") %>% select(subject_id, excl03)
x <- lb %>% filter(redcap_event_name == "screening_arm_1") %>% select(subject_id, redcap_event_name) %>%
  left_join(ccr, by = "subject_id") %>%
  left_join(excl, by = "subject_id") %>%
  filter(eccr < 60 & excl03 == 0)
add_findings(x, "laboratory", "eccr", "Screening eCCr below 60 but exclusion 3 = No",
             value = paste0("eCCr ", x$eccr, " / excl03 = No"))
# Expected: 1 row

# --- Check 37: antidiabetic medicine in the concomitant medication list ---
# The study needs treatment-naive subjects, so these drugs are not allowed
# before randomisation. tolower() makes "Metformin" and "metformin" match.
# Expected: 1 row
antidiab <- c("metformin", "insulin", "glimepiride", "gliclazide", "sitagliptin",
              "empagliflozin", "dapagliflozin", "pioglitazone")
x <- cm %>% filter(tolower(cmtrt) %in% antidiab)
add_findings(x, "concomitant_medications", "cmtrt", "Antidiabetic medication in con-med list")

# --- Check 38: AE says drug withdrawn, but the subject is marked Completed ---
# aeacn: 3 = Drug withdrawn.  dsdecod: 1 = Completed.
# Expected: 1 row
withdrawn <- ae %>% filter(aeacn == 3) %>% select(subject_id, aeacn)
x <- ds %>% filter(dsdecod == 1) %>% semi_join(withdrawn, by = "subject_id")
add_findings(x, "disposition", "dsdecod", "AE drug withdrawn but subject marked completed",
             value = "AE action = drug withdrawn / disposition = completed")

# --- Check 39: clinically significant lab value but NO adverse event at all ---
# lbclsig: 1 = clinically significant. anti_join keeps subjects with no AE record.
# Expected: 1 row
x <- lb %>% filter(lbclsig == 1) %>% anti_join(ae, by = "subject_id")
add_findings(x, "laboratory", "lbclsig", "Clinically significant lab result with no AE reported")

# ---------------------------------------------------------------------
# YOUR CHECKS GO HERE (copy the pattern above)
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# PART 5: Save the discrepancy listing
# ---------------------------------------------------------------------
# dir.create() makes the folder if it does not exist yet.
dir.create("05_Queries", showWarnings = FALSE)

# arrange() sorts the listing by subject and then form.
findings <- findings %>% arrange(subject_id, form)

# write.csv() saves the table as a file you can open in Excel.
write.csv(findings, "05_Queries/discrepancy_listing.csv", row.names = FALSE)

# Print how many discrepancies were found. With only the 7 starter checks
# above, this should print 7.
nrow(findings)


# ---------------------------------------------------------------------
# PART 6: Build the query log from the discrepancy listing
# ---------------------------------------------------------------------

# A named vector works as a lookup: give it a code, it returns the label.
# Left side = the REDCap event name, right side = what the site sees.
visit_label <- c(screening_arm_1 = "Screening", runin_arm_1 = "Run-in",
                 baseline_arm_1 = "Baseline",   week12_arm_1 = "Week 12",
                 week24_arm_1   = "Week 24")

query_log <- findings %>%
  arrange(subject_id, form, visit) %>%
  
  mutate(
    # sprintf("Q%03d", 7) builds "Q007": Q + the number padded to 3 digits.
    query_id    = sprintf("Q%03d", row_number()),
    
    # visit_label[visit] looks up the friendly name.
    # coalesce() keeps the original text if the lookup finds nothing.
    visit_name  = coalesce(unname(visit_label[visit]), visit),
    
    # The text the site will read. It states the problem and the value,
    # and asks the site to CONFIRM OR CORRECT it. It must never tell the
    # site what the answer should be (a "leading query").
    query_text  = paste0("Subject ", subject_id, " - ", form, " (", visit_name, "): ",
                         check, ". Value entered: ", value = coalesce(value, "(blank)"),
                         ". Please review the source document and confirm or correct."),
    
    raised_by     = "Dheerajh Goud",
    date_raised   = as.character(Sys.Date()),
    status        = "Open",              # Open -> Answered -> Closed
    site_response = "",                  # the site fills this in
    resolution    = "",                  # what you did with the answer
    date_closed   = ""
  ) %>%
  
  # select() chooses the columns and sets their order in the final file.
  select(query_id, subject_id, form, visit = visit_name, variable, value,
         check, query_text, raised_by, date_raised, status,
         site_response, resolution, date_closed)

# write_xlsx() comes from the writexl package you already loaded.
write_xlsx(query_log, "05_Queries/query_log.xlsx")

# Check: this should print 41, and the file should open in Excel.
nrow(query_log)


# ---------------------------------------------------------------------
# PART 7: Medical coding (mock dictionaries)
# ---------------------------------------------------------------------
# tribble() lets you type a small table row by row, like writing it on paper.
# "~" marks the column names. One row = one verbatim term and its coded values.
ae_dict <- tribble(
  ~aeterm,                    ~pt_term,                      ~soc_term,
  "increased urination",      "Pollakiuria",                 "Renal and urinary disorders",
  "headache",                 "Headache",                    "Nervous system disorders",
  "UTI",                      "Urinary tract infection",     "Infections and infestations",
  "cold and cough",           "Upper respiratory tract infection", "Infections and infestations",
  "low blood sugar",          "Hypoglycaemia",               "Metabolism and nutrition disorders",
  "loose stools",             "Diarrhoea",                   "Gastrointestinal disorders",
  "nausea",                   "Nausea",                      "Gastrointestinal disorders",
  "dizziness",                "Dizziness",                   "Nervous system disorders",
  "yeast infection",          "Genital infection fungal",    "Infections and infestations",
  "pneumonia - hospitalised", "Pneumonia",                   "Infections and infestations",
  "appendicitis, surgery",    "Appendicitis",                "Gastrointestinal disorders",
  "back pain",                "Back pain",                   "Musculoskeletal and connective tissue disorders",
  "persistent vomiting",      "Vomiting",                    "Gastrointestinal disorders"
)
# NOTE: "unwell" is deliberately NOT in the dictionary. It is too vague to
# code, so it stays uncoded until the site answers query Q036.

# left_join() attaches pt_term and soc_term to each AE by matching the verbatim.
# Verbatims with no match get NA, which is how we find the uncoded ones.
ae_coded <- ae %>% left_join(ae_dict, by = "aeterm")

# Which AEs could not be coded? (this should list the one "unwell" row)
ae_coded %>% filter(is.na(pt_term)) %>% select(subject_id, aeterm)

# How many events per coded term and body system?
ae_coded %>% count(soc_term, pt_term, sort = TRUE)

# --- Concomitant medications: WHO-Drug-style name + ATC code ---
cm_dict <- tribble(
  ~cmtrt,         ~drug_name,        ~atc_code,
  "Lisinopril",   "Lisinopril",      "C09AA03",
  "Amlodipine",   "Amlodipine",      "C08CA01",
  "Vitamin D3",   "Colecalciferol",  "A11CC05",
  "Atorvastatin", "Atorvastatin",    "C10AA05",
  "Aspirin",      "Acetylsalicylic acid", "B01AC06",
  "Paracetamol",  "Paracetamol",     "N02BE01",
  "Omeprazole",   "Omeprazole",      "A02BC01",
  "Metformin",    "Metformin",       "A10BA02"
)
cm_coded <- cm %>% left_join(cm_dict, by = "cmtrt")
cm_coded %>% filter(is.na(drug_name)) %>% select(subject_id, cmtrt)   # should be empty

# Save both coded tables for your portfolio
write_xlsx(list(AE_coded = ae_coded, CM_coded = cm_coded), "05_Queries/coded_data.xlsx")

# ---------------------------------------------------------------------
# PART 8: SAE reconciliation (EDC vs safety database)
# ---------------------------------------------------------------------
f_safety <- "03_Data/SAE_Safety_Listing_Mock.xlsx"
safety <- read_excel(f_safety, sheet = "safety_listing")

# Side 1: the serious AEs from the EDC (aeser == 1).
# rename: "new_name = old_name". The edc_ prefix keeps the two sides apart.
edc_sae <- ae_coded %>%
  filter(aeser == 1) %>%
  select(subject_id, edc_term = pt_term, edc_onset = aestdat,
         edc_outcome = aeout, edc_crit = aesercrit)

# full_join() keeps rows from BOTH tables, matched on subject_id.
# If an SAE exists on only one side, the other side's columns become NA.
# (Simplification: one SAE per subject. A real study also matches on term/onset.)
recon <- full_join(edc_sae, safety, by = "subject_id") %>%
  mutate(
    # TRUE only when the SAE exists on BOTH sides, so we can compare fields.
    both = !is.na(edc_onset) & !is.na(sd_onset),
    
    # paste0() glues the pieces together. Each ifelse() adds text only when
    # its problem applies, so one row can list several issues.
    issues = paste0(
      ifelse(is.na(edc_onset), "SAE not in EDC; ", ""),
      ifelse(is.na(sd_onset),  "SAE not in safety database; ", ""),
      ifelse(both & as.Date(edc_onset) != as.Date(sd_onset), "Onset date differs; ", ""),
      ifelse(both & edc_outcome != sd_outcome, "Outcome differs; ", ""),
      # coalesce(edc_crit, -1) turns a blank criterion into -1, so blank counts as different.
      ifelse(both & coalesce(edc_crit, -1) != sd_crit, "Seriousness criterion differs; ", "")
    )
  ) %>%
  filter(issues != "") %>%
  select(subject_id, case_id, edc_term, sd_term, edc_onset, sd_onset,
         edc_outcome, sd_outcome, edc_crit, sd_crit, issues)

recon          # view it
write_xlsx(recon, "05_Queries/sae_reconciliation.xlsx")
nrow(recon)    # expect 3


# ---------------------------------------------------------------------
# PART 8b: Add the SAE reconciliation queries to the query log
# ---------------------------------------------------------------------
sae_queries <- tribble(
  ~subject_id, ~visit,    ~variable,   ~value,                           ~check,
  ~query_text,
  "S007", "Week 24", "aestdat",
  "EDC 2026-07-04 / safety DB 2026-07-03",
  "SAE reconciliation: onset date differs",
  "Subject S007 - adverse_events (Week 24): the onset date of the serious AE 'pneumonia' is 04-Jul-2026 in the EDC but 03-Jul-2026 in the safety database (case SD-0001). Please review the source documents and confirm or correct the onset date. (The blank seriousness criterion is queried separately under Q012.)",
  
  "S011", "Week 24", "aeout",
  "EDC Recovered / safety DB Recovering",
  "SAE reconciliation: outcome differs",
  "Subject S011 - adverse_events (Week 24): the outcome of the serious AE 'appendicitis' is 'Recovered' in the EDC but 'Recovering' in the safety database (case SD-0002). Please review the source documents and confirm or correct the outcome.",
  
  "S018", "n/a", "adverse_events",
  "(not entered)",
  "SAE reconciliation: SAE not in EDC",
  "Subject S018: the safety database contains a serious AE (case SD-0003, ankle fracture, onset 02-May-2026) that is not recorded on the Adverse Events form in the EDC. Please review the source documents and enter the event if applicable, or clarify."
) %>%
  mutate(form          = "adverse_events",
         raised_by     = "Dheerajh Goud",
         date_raised   = as.character(Sys.Date()),
         status        = "Open",
         site_response = "", resolution = "", date_closed = "")

# bind_rows() stacks the new queries under the old ones.
# Then query_id is rebuilt in order, so the new queries become Q042-Q044.
query_log <- bind_rows(query_log, sae_queries) %>%
  mutate(query_id = sprintf("Q%03d", row_number())) %>%
  select(query_id, subject_id, form, visit, variable, value, check,
         query_text, raised_by, date_raised, status,
         site_response, resolution, date_closed)

write_xlsx(query_log, "05_Queries/query_log.xlsx")
nrow(query_log)   # expect 44


# ---------------------------------------------------------------------
# PART 9: SDTM DM domain (one row per subject)
# ---------------------------------------------------------------------
f_rand <- "03_Data/Randomisation_List_Mock.xlsx"
arms   <- read_excel(f_rand, sheet = "randomisation_list")

studyid <- "MOCK-NCT01719003"

# First and last dose date per subject (from the drug exposure form).
# if (all(is.na(...))) handles a subject with NO dates (S022 has no
# last-dose date). Without it, min()/max() of nothing returns Inf and a warning.
dose_dates <- ex %>%
  group_by(subject_id) %>%
  summarise(
    first_dose = if (all(is.na(exstdat))) NA_character_ else as.character(as.Date(min(exstdat, na.rm = TRUE))),
    last_dose  = if (all(is.na(exendat))) NA_character_ else as.character(as.Date(max(exendat, na.rm = TRUE)))
  )

dm_sdtm <- dm %>%
  # distinct() keeps ONE row per subject (drops the duplicate S006 record).
  # In a real study you would wait for query Q009 to be resolved first.
  distinct(subject_id, .keep_all = TRUE) %>%
  left_join(consent,    by = "subject_id") %>%    # adds icdat
  left_join(dose_dates, by = "subject_id") %>%    # adds first_dose, last_dose
  left_join(arms,       by = "subject_id") %>%    # adds armcd, arm
  
  # transmute() builds the NEW columns and drops everything else,
  # in the order you list them (SDTM expects a fixed order).
  transmute(
    STUDYID = studyid,
    DOMAIN  = "DM",
    USUBJID = paste(studyid, "001", subject_id, sep = "-"),   # 001 = mock site
    SUBJID  = subject_id,
    RFICDTC = as.character(as.Date(icdat)),
    RFSTDTC = first_dose,
    RFENDTC = last_dose,
    SITEID  = "001",
    AGE     = age,
    AGEU    = "YEARS",
    SEX     = sex,
    # case_when() works like a lookup: first TRUE condition wins.
    # We write the controlled-terminology text in capitals, as CDISC does.
    RACE = case_when(race == 1 ~ "AMERICAN INDIAN OR ALASKA NATIVE",
                     race == 2 ~ "ASIAN",
                     race == 3 ~ "BLACK OR AFRICAN AMERICAN",
                     race == 4 ~ "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
                     race == 5 ~ "WHITE",
                     race == 6 ~ "OTHER",
                     race == 7 ~ "NOT REPORTED",
                     TRUE      ~ NA_character_),
    ETHNIC = case_when(ethnic == 1 ~ "HISPANIC OR LATINO",
                       ethnic == 2 ~ "NOT HISPANIC OR LATINO",
                       ethnic == 3 ~ "NOT REPORTED",
                       ethnic == 4 ~ "UNKNOWN",
                       TRUE        ~ NA_character_),
    ARMCD   = armcd,
    ARM     = arm,
    COUNTRY = "IND"
  )

# --- Checks on your own work ---
nrow(dm_sdtm)                        # expect 30
ncol(dm_sdtm)                        # expect 16
dm_sdtm %>% count(ARMCD, ARM)        # expect 10 subjects in each of 3 arms
dm_sdtm %>% filter(is.na(SEX) | is.na(RFENDTC) | AGE < 18) %>%
  select(USUBJID, SEX, AGE, RFENDTC) # expect S026 (no sex), S027 (age 16), S022 (no last dose)

dir.create("06_SDTM", showWarnings = FALSE)
write_xlsx(list(DM = dm_sdtm), "06_SDTM/DM.xlsx")
write.csv(dm_sdtm, "06_SDTM/dm.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# PART 10: SDTM AE domain (one row per adverse event)
# ---------------------------------------------------------------------

# Study day = days from the first dose. SDTM has NO day 0:
#   the first-dose day is Day 1, the day before is Day -1.
# as.Date() turns the text into a date; subtracting gives the difference.
study_day <- function(date, ref) {
  d <- as.numeric(as.Date(date) - as.Date(ref))
  ifelse(is.na(d), NA, ifelse(d >= 0, d + 1, d))
}

ae_sdtm <- ae_coded %>%
  # Bring USUBJID and the first dose date from the DM domain we just built.
  left_join(dm_sdtm %>% select(SUBJID, USUBJID, RFSTDTC),
            by = c("subject_id" = "SUBJID")) %>%
  # AESEQ = 1, 2, 3 ... within each subject, in order of start date.
  group_by(USUBJID) %>%
  arrange(aestdat, .by_group = TRUE) %>%
  mutate(AESEQ = row_number()) %>%
  ungroup() %>%
  transmute(
    STUDYID  = studyid,
    DOMAIN   = "AE",
    USUBJID  = USUBJID,
    AESEQ    = AESEQ,
    AETERM   = aeterm,        # verbatim, exactly what the site wrote
    AEDECOD  = pt_term,       # coded Preferred Term (blank if uncoded)
    AEBODSYS = soc_term,      # coded body system
    AESEV = case_when(aesev == 1 ~ "MILD", aesev == 2 ~ "MODERATE",
                      aesev == 3 ~ "SEVERE", TRUE ~ NA_character_),
    AESER = case_when(aeser == 1 ~ "Y", aeser == 0 ~ "N", TRUE ~ NA_character_),
    AEREL = case_when(aerel == 1 ~ "NOT RELATED",
                      aerel == 2 ~ "UNLIKELY RELATED",
                      aerel == 3 ~ "POSSIBLY RELATED",
                      aerel == 4 ~ "PROBABLY RELATED",
                      aerel == 5 ~ "RELATED", TRUE ~ NA_character_),
    AEACN = case_when(aeacn == 1 ~ "DOSE NOT CHANGED",
                      aeacn == 2 ~ "DRUG INTERRUPTED",
                      aeacn == 3 ~ "DRUG WITHDRAWN",
                      aeacn == 4 ~ "NOT APPLICABLE", TRUE ~ NA_character_),
    AEOUT = case_when(aeout == 1 ~ "RECOVERED/RESOLVED",
                      aeout == 2 ~ "RECOVERING/RESOLVING",
                      aeout == 3 ~ "NOT RECOVERED/NOT RESOLVED",
                      aeout == 4 ~ "RECOVERED/RESOLVED WITH SEQUELAE",
                      aeout == 5 ~ "FATAL",
                      aeout == 6 ~ "UNKNOWN", TRUE ~ NA_character_),
    # One flag per seriousness criterion. Blank (NA) when not applicable.
    AESDTH  = if_else(!is.na(aesercrit) & aesercrit == 1, "Y", NA_character_),  # death
    AESLIFE = if_else(!is.na(aesercrit) & aesercrit == 2, "Y", NA_character_),  # life-threatening
    AESHOSP = if_else(!is.na(aesercrit) & aesercrit == 3, "Y", NA_character_),  # hospitalisation
    AESDISAB= if_else(!is.na(aesercrit) & aesercrit == 4, "Y", NA_character_),  # disability
    AESCONG = if_else(!is.na(aesercrit) & aesercrit == 5, "Y", NA_character_),  # congenital anomaly
    AESMIE  = if_else(!is.na(aesercrit) & aesercrit == 6, "Y", NA_character_),  # other medically important
    AESTDTC = as.character(as.Date(aestdat)),
    AEENDTC = as.character(as.Date(aeendat)),
    AESTDY  = study_day(aestdat, RFSTDTC),
    AEENDY  = study_day(aeendat, RFSTDTC)
  )

# --- Checks on your own work ---
nrow(ae_sdtm)                                    # expect 37
ncol(ae_sdtm)                                    # expect 24
ae_sdtm %>% count(AESER)                         # expect N = 35, Y = 2
ae_sdtm %>% filter(AESTDY < 0) %>%               # events BEFORE the first dose
  select(USUBJID, AETERM, AESTDY)                # expect 4 rows
ae_sdtm %>% filter(is.na(AEDECOD)) %>%           # uncoded events
  select(USUBJID, AETERM)                        # expect 1 row: unwell

write_xlsx(list(AE = ae_sdtm), "06_SDTM/AE.xlsx")
write.csv(ae_sdtm, "06_SDTM/ae.csv", row.names = FALSE)

