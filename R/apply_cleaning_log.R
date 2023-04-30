# clean the cleaning log -----------------------------------------------------------------
options(scipen = 999)
#Filter empty rows
correction_log_filtered <- correction_log %>% 
  filter(!(is.na('Id') & is.na(Question) & is.na(old_value)))

#identify issues
correction_log_filtered <- correction_log_filtered %>% 
  mutate(issue = case_when(
    Question %notin% names(pdm_data) ~ "question",
    Id %notin% pdm_data$`_id` ~ "Id"
    ))

correction_log_filtered$duplicates <- duplicated(correction_log_filtered[, c("Id", "Question")], fromLast = T) | 
  duplicated(correction_log_filtered[, c("Id", "Question")])

correction_log_issues <- correction_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>% 
  arrange(Id, Question)

correction_log_filtered <- correction_log_filtered %>% 
  filter(is.na(issue))

# apply the correction-log -------------------------------------------
pdm_data_copy <- pdm_data
pdm_data <- apply_log(data = pdm_data, log = correction_log_filtered, data_KEY = "_id",
                                             log_columns = c(question = "Question",
                                                             old_value = "old_value",
                                                             new_value = "new_value",
                                                             KEY = "Id"))

# Verify correction log -------------------------------------------
message("Verifying Correction log, please wait!")
correction_log_discrep <- compare_dt(df1 = pdm_data_copy, df2 = pdm_data, unique_id_df1 = "_id", unique_id_df2 = "_id")

# Removing extra spaces from new_value before joining 
correction_log_discrep <- correction_log_discrep %>%
  mutate(Id = as.character(KEY), KEY = NULL) %>% 
  anti_join(correction_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("Id", "question"="Question", "new_value"))

# remove extra objects -------------------------------------------
rm(pdm_data_copy, correction_log_filtered)

