# Mock Clinical Data Management Study (based on NCT01719003)

An end-to-end clinical data management (CDM) project built from scratch on **dummy data**:
eCRF build, edit checks, query management, medical coding, SAE reconciliation,
SDTM mapping and a database lock checklist.

> **All data are dummy data.** No real patient information is used. The study design is based on the
> public trial [NCT01719003](https://clinicaltrials.gov/study/NCT01719003)
> (empagliflozin + metformin in type 2 diabetes, 24 weeks). This project is not affiliated with the trial sponsor.

**Full report:** open `CDM-report.html` (or the live page, if GitHub Pages is enabled: *add link here*).

## Key results

| Area | Result |
|---|---|
| eCRF | 9 forms, 5 visits, 79 fields built in REDCap, with calculated fields, branching logic and range checks |
| Data | 30 dummy subjects with 39 deliberately seeded errors |
| Edit checks | 39 checks programmed in R (dplyr): range, logic, missing data, duplicates, cross-form date and consistency checks |
| Validation | **39 of 39 seeded errors detected, 0 false positives** (41 discrepancy rows, because 2 errors involve two subjects) |
| Queries | 44 queries (41 from edit checks, 3 from SAE reconciliation), written as non-leading queries |
| Coding | 37 adverse events and 58 medications coded with a mock MedDRA-style and WHO-DD/ATC-style dictionary; 1 term left uncoded as too vague (queried) |
| SAE reconciliation | EDC vs a mock safety database: 3 discrepancies found (onset date, outcome, missing SAE) |
| SDTM | DM (30 rows) and AE (37 rows) domains, ISO 8601 dates, study days, controlled terminology |
| Database lock | Checklist of 23 items: 7 done, 16 open, so **not ready to lock** (44 queries unanswered) |

## Workflow

| Phase | What I did | Where |
|---|---|---|
| 1. Study start-up | Chose a public trial, wrote a CRF specification, built the eCRF in REDCap | `02_REDCap_backup/` |
| 2. Data generation | Created a 30-subject dummy dataset with a hidden answer key of seeded errors | `03_Data/` |
| 3. Validation | Wrote 39 R edit checks that build a discrepancy listing | `04_Code/validations_check.R` |
| 4. Query management | Turned discrepancies into a query log | `05_Queries/` |
| 5. Coding | Mapped verbatim terms to preferred terms, body systems and ATC codes | `04_Code/`, `05_Queries/coded_data.xlsx` |
| 6. SAE reconciliation | Compared EDC serious AEs with a safety database listing | `05_Queries/sae_reconciliation.xlsx` |
| 7. SDTM | Mapped DM and AE to SDTM | `06_SDTM/` |
| 8. Close-out | Database lock checklist | `Database_Lock_Checklist_Mock.xlsx` |

## How to reproduce

1. Install R and RStudio, then the packages: `install.packages(c("readxl", "dplyr", "tidyr", "purrr", "writexl", "rmarkdown", "knitr"))`
2. Open the project folder in RStudio.
3. Run `04_Code/validations_check.R`, or knit `CDM-report.Rmd` to rebuild the report (it runs the script).

## Tools and skills

REDCap, R (dplyr, readxl, writexl), R Markdown, SDTM / CDISC concepts, ICH-GCP and ALCOA+ principles,
discrepancy management, medical coding concepts (MedDRA, WHO Drug), SAE reconciliation.

## Limitations (stated plainly)

- Data and the safety database are **simulated**; real data would be messier.
- MedDRA and WHO Drug are licensed dictionaries. This project uses a small **mock dictionary written for the study** and does not contain any licensed content.
- SAE reconciliation assumes one SAE per subject; a real study also matches on term and onset date.
- SDTM datasets are built from pre-lock data as a dry run, and do not include define.xml or validation with Pinnacle 21.

## Author

Dheerajh Goud, Pharm.D. | LinkedIn: [linkedin.com/in/dheerajh-goud-451204218](https://www.linkedin.com/in/dheerajh-goud-451204218)
