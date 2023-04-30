##### Data Processing Script #####
# Install/load required packages -----------------------------------------------------------------
if(!require(devtools)) install.packages("devtools")
if(!require(atRfunctions)) install_github("atrcodebase/atRfunctions")
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(glue)) install.packages("glue")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(writexl)) install.packages("writexl")
source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Download data --------------------------------------------------------------------------
# file.edit("R/download_data.R")
# source("R/download_data.R")

# Read data ------------------------------------------------------------------------------
data_path <- "input/raw_data/" # data path
files <- list.files(data_path, pattern = "Post_Distribution_Monitoring_for_GIHA_Cash_Project")
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA
pdm_data <- read_excel(paste0(data_path, files), guess_max = 100000, na = convert_to_na)
# Relevancy Rule
pdm_tool_relevancy <- read_excel("input/tool_relevancy_rules/PDM_tool_relevancies.xlsx")

janitor::get_dupes(pdm_data, "_id")

# read qa-log, correction log, and translation log -------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRc7TjvA-A66cbF4SNcgqSd-0siKkndsi1ps1EpJcTDO4zdx6R5SXvRO4cPztyUzbGgWdf0pnlCd6JH/pub?"
qa_log <- readr::read_csv(paste0(url, "gid=0&single=true&output=csv"), col_types = "c")
correction_log <- readr::read_csv(paste0(url, "gid=108934644&single=true&output=csv"), col_types = "c")
translation_log <- readr::read_csv(paste0(url, "gid=488178430&single=true&output=csv"), col_types = "c")

# reformat Dates
correction_log <- correction_log %>% 
  # filter(Question %in% c("start", "end")) %>% 
  mutate(new_value = case_when(
    Question %in% c("start", "end") ~ as.character(as.POSIXct(new_value, format = "%m/%d/%Y %I:%M:%S %p")),
    TRUE ~ new_value),
    .after = new_value)

# apply correction log -------------------------------------------
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R")
if(nrow(correction_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

# Update Select_multiple series columns ------------------------------------------------------------
multi_vars <- c("How_did_you_get_to_cash_distro_point", "Transportation_challenges", "Spend_on_what")
pdm_data <- update_series_cols(pdm_data, multi_vars=multi_vars,"/") 

# Relevancy check ----------------------------------------------------------------------------------
# Check Relevancy Rules
pdm_issues <- check_relevancy_rules(pdm_data, pdm_tool_relevancy)
# Check Select_Multiple Questions
pdm_SM_issues = check_select_multiple(pdm_data, multi_vars, separator="/", KEY="_id")
#check
pdm_issues
pdm_SM_issues

# attach value labels ------------------------------------------
tool_path <- "input/tools/Post Distribution Monitoring for GIHA Cash Project.xlsx"
# apply the value labels
pdm_data <- labeler(data = pdm_data,
                             tool = tool_path,
                             survey_label = "label::English(EN)",
                             choice_lable = "label::English(EN)",
                             multi_response_sep = ";")

# apply translation log -------------------------------------------
# file.edit("R/apply_translation_log.R")
source("R/apply_translation_log.R")
if(nrow(translation_log_discrep) !=0){
  print("Translation Logs not applied -------------------")
  translation_log_discrep
}

# recode variables -----------------------------------------------------------------------
## use this file in case if we need to modify some variables
# file.edit("R/recode.R")
source("R/recode.R")

# remove extra columns -------------------------------------------------------------------
{
extra_cols <- c("deviceid", "audit", "survey", "background-audio", "audit_URL",	"background-audio_URL", 
                "Username", "Password", "Surveyor_Name", "Check_Password", "Valid_Credentials", 
                "Invalid_credentials", "Surveyor_Name_label", "Form_access", 
                "Enumerator_Name", "Resp_ID", "caseid", "call_num", "cur_call_num", "pre_call_num", "last_call_status", 
                "callback_time", "Sampled_Respondent_Name", "Sampled_Respondent_FName", 
                "Admn1Code", "Admn2Code", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9", "n12", "n13", "n14", "n15", "n16", 
                "n17", "now_complete", "nc_final", "closed", "Resp_pn", "Resp_pn0", 
                "phone_response", "Consent", "phone_number_new_case", "note31", "n58", "n59", "n60", "n61",
                "_validation_status",	"_notes",	"_status",	"_submitted_by",	"__version__",	"_tags",	"_index",
                # Redundent cols
                "cashproject", "CID_Number", "PFirst_Name", "PFamily_Name", 
                  "Pprovince", "Pdistrict", "Pvillage", "Plocation0", "Plocation", 
                  "PAge_Group", "PWoman_HH", "PTarget_Group", "How_did_you_get_to_t_bution_point_of_cash", 
                  "How_did_you_get_to_t_bution_point_of_cash/walking", "How_did_you_get_to_t_bution_point_of_cash/public_transport", 
                  "How_did_you_get_to_t_bution_point_of_cash/private_vehicle", 
                  "How_did_you_get_to_t_bution_point_of_cash/taxi", "How_did_you_get_to_t_bution_point_of_cash/animal", 
                  "Is_she_Woman_HH_Yes_No", "Do_you_know_how_much_y_you_received_today", 
                  "Age_Group_of_the_woman", "Do_you_know_the_purpose_of_cash_received", 
                  "Passcode", "How_much_did_you_pay_ation_Afg_currency", "Any_challenges_about_the_transportation", 
                  "How_did_you_get_the_n_to_come_here_today", "How_long_did_it_take_eive_your_cash_today", 
                  "How_satisfied_are_yo_s_and_partners_today", "Did_the_cash_distribution_proc", 
                  "If_yes_please_expla_to_protect_yourself", "Do_you_know_how_to_m_edback_if_threatened", 
                  "_version_", "If_yes_please_clarify_how", "Did_you_spend_on_your_grant_re", 
                  "What_do_you_plan_to_y_you_received_today", "If_others_please_clarify_001", 
                  "If_No_explain_why_y_not_spent_the_money", "Did_you_have_full_decision_on_", 
                  "If_No_who_had_the_decision", "n10", "n11")
}

pdm_data <- pdm_data %>% select(-all_of(extra_cols))

# produce qa-backlog ---------------------------------------------------------------------
qa_log_sub <- qa_log %>% select(qa_status=`QA status`, `_id`) %>% mutate(`_id`=as.integer(`_id`))
## Filter
QA_backlog_keys <- left_join(
  pdm_data %>% filter(choice_ans == "Complete") %>% select(submission_date, `_id`), qa_log_sub, by = "_id") %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>% filter(qa_status %notin% c("Approved", "Rejected"))
QA_backlog <- QA_backlog_keys %>% 
  group_by(submission_date) %>% 
  count(qa_status, name = "freq") %>% 
  mutate(percent_Host = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% 
  arrange(submission_date)
# Print
print(knitr::kable(QA_backlog, format = "simple"))

# remove Rejected keys ---------------------------------------------------------
count(qa_log, `QA status`)
pdm_data <- pdm_data %>% filter(qa_status %notin% "Rejected")


# generate data with missing translations ------------------------------------------------
excluded_cols <- c("Date_And_Time", "Village_New", "Village_Final", "Reason_Not_reached_out_to_respondent_Other")
missing_translation_log <- missing_translation(data = pdm_data, KEY = "_id", excluded_cols)


# Filter Approved Data only --------------------------------------------------------------
pdm_data_filtered <- pdm_data %>% 
  filter(qa_status %in% "Approved")

# Export ---------------------------------------------------------------------------------
# QA Backlog
qa_backlog_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)
log_issues <- list(
  correction_log_issues=correction_log_issues,
  translation_log_issues=translation_log_issues
  )
Logs <- list(
  correction_log=correction_log,
  translation_log=translation_log
)

## create the output path
check_path("output/cleaned_data")
## export cleaned datasets
writexl::write_xlsx(list("Post Distribution Monitoring..."=pdm_data), "output/cleaned_data/Post_Distribution_Monitoring_for_GIHA_Cash_Project_Cleaned.xlsx", format_headers = F) # Cleaned
writexl::write_xlsx(list("Post Distribution Monitoring..."=pdm_data_filtered), "output/cleaned_data/Post_Distribution_Monitoring_for_GIHA_Cash_Project_Cleaned_Approved.xlsx", format_headers = F) # Cleaned & Approved

## keep a copy of correct & translation log, export log issues, export missing translation, etc.
writexl::write_xlsx(Logs, "output/correction_translation_log.xlsx", format_headers = F) # correction & translation log
writexl::write_xlsx(log_issues, "output/Log_issues.xlsx", format_headers = F) # correction & Translation log issues
writexl::write_xlsx(qa_backlog_list, "output/QA_Backlog_by_Date.xlsx", format_headers = F)
writexl::write_xlsx(translation_log_discrep, "output/translation_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(correction_log_discrep, "output/correction_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_log, "output/untranslated_log.xlsx", format_headers = F)

