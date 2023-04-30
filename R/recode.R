# split 'starttime' and 'endtime' columns into separate 'date' and 'time' columns; calculate time difference -------------------------------------------

numeric_cols <- c("Age", "How_much_did_you_pay_for_transportation_in_AFN")

remove_98_99 <- function(x) {
  x = case_when(
    x %in% c(999) ~ as.numeric(NA),
    TRUE ~ x
  )}

# Join QA Status -------------------------------------------------------------------------
qa_log_sub <- qa_log %>% 
  select(qa_status=`QA status`, `_id`) %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "Pending",
    TRUE ~ qa_status), `_id`=as.integer(`_id`)) %>% unique()

pdm_data <- pdm_data %>% left_join(qa_log_sub, by="_id")

# UN Women PDM Dataset ---------------------------------------------------------
pdm_data <- pdm_data %>% 
  mutate(id=Enumerator_ID, Enumerator_ID=Enumerator_Gender, Enumerator_Gender=id) %>% # Swapping Id and gender
  mutate(start = str_replace(start, "T", " "),
         end = str_replace(end, "T", " "),
         `_submission_time` = str_replace(`_submission_time`, "T", " ")) %>% 
  mutate(
    startdate = as.Date(start),
    enddate = as.Date(end),
    starttime = paste(hour(start), minute(start), sep = ":"),
    endtime = paste(hour(end), minute(end), sep = ":"),
    submission_date = as.Date(`_submission_time`), 
    across(all_of(numeric_cols), as.numeric)) %>%
  select(startdate, enddate, starttime, endtime, submission_date, everything(), -c(start, end, id))

# Calculate new variables
pdm_data <- pdm_data %>% 
  mutate(District_Final = case_when(
    !is.na(District_New) ~ District_New,
    TRUE ~ District
  ), Village_Final = case_when(
    !is.na(Village_New) ~ Village_New,
    TRUE ~ Village), .after=Province_Final) %>% 
  mutate(age_groups_Final = case_when(
    Age >= 12 & Age <= 17 ~ "12 - 17 Years",
    Age >= 18 & Age <= 59 ~ "18 - 59 Years",
    Age >= 60 & Age != 999 ~ "60+",
    Age == 999 ~ "I don't know"
  ), .after = Age) %>% 
  mutate(across(all_of(numeric_cols), remove_98_99))

# Age < 18 ~ "Lower than 18",
# Age >= 18 & Age <= 24 ~ "18 - 24 Years",
# Age >= 25 & Age <= 29 ~ "25 – 29 Years",
# Age >= 30 & Age <= 44 ~ "30 – 44 Years",
# Age >= 45 & Age <= 59 ~ "45 – 59 Years",
# Age >= 60 ~ "60+"

# Remove extra objects -------------------------------------------------------------------
rm(numeric_cols, remove_98_99, qa_log_sub)
