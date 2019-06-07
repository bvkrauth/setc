## code to prepare `DATASET` dataset goes here
library("tidyverse")

econ_questions <- readr::read_csv("data-raw/econ question list.csv") %>%
  dplyr::select(question_id,
                question_text,
                question_level,
                table_type,
                label_1,
                label_2,
                label_3,
                label_4,
                label_5)

fass_questions <- econ_questions %>%
  dplyr::filter(question_level == "SFU" | question_level == "FASS")

sfu_questions <- econ_questions %>%
  dplyr::filter(question_level == "SFU")

# Here is some code I wanted to use.  It saves the questions
# in R/sysdata.rda so that they can be used within the package
# as data.  However, that introduces a dependency (R >= 2.09)
# that gets flagged with a warning by R CMD CHECK, so I
# am leaving it out.

#usethis::use_data(sfu_questions, fass_questions,
#                  internal = TRUE,
#                  overwrite = TRUE)
