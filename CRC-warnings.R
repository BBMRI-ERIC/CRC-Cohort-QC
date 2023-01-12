source("CRC-functions.R")

# Reporting

w <- rows_patients %>% dplyr::filter(fromDiagToVitalCheckWeeks < 0) %>% dplyr::mutate(warningType = "Vital check date precedes initial diagnosis date", warning = paste("Date of last vital check ", vital_status_timestamp, " precedes initial diagnosis date ", diag_date, sep="")) %>% select(locationName, id, patientid, warningType, warning) 
CRCwarnings <- w

w <- rows_patients %>% dplyr::filter(fromDiagToVitalCheckWeeks == 0) %>% dplyr::mutate(warningType = "Vital check date is equal to initial diagnosis date", warning = paste("Date of last vital check ", vital_status_timestamp, " equals to initial diagnosis date ", diag_date, sep="")) %>% select(locationName, id, patientid, warningType, warning) 
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_patients %>% dplyr::filter(fromDiagToVitalCheckWeeks > 0, survivalToVitalRatio > 3) %>% dplyr::mutate(warningType = "Suspicious survival information", warning = paste("Provided survival is ", overall_survival, " weeks while difference between initial diagnosis and last vital check is ", round(fromDiagToVitalCheckWeeks, digits=0), " weeks", sep="")) %>% select(locationName, id, patientid, warningType, warning) 
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_patients %>% dplyr::filter(age_at_primary_diagnosis < 15) %>% dplyr::mutate(warningType = "Suspiciously young patient", warning = paste("Age of patient at initial diagnosis is ", age_at_primary_diagnosis, " years", sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_patients %>% dplyr::filter(overall_survival > 4000 | (age_at_primary_diagnosis + overall_survival/52 >= 100 & age_at_primary_diagnosis < 95)) %>% dplyr::mutate(warningType = "Suspiciously long survival", warning = paste("Patient was diagnosed at age of ", age_at_primary_diagnosis, " years and the resulting survival of ", overall_survival, " weeks means surviving is until ", round(age_at_primary_diagnosis + overall_survival/52, digits=1), " years", sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_patients %>% dplyr::filter(vital_status != "UNKNOWN" & is.na(vital_status_timestamp)) %>% dplyr::mutate(warningType = "Vital status timestamp missing", warning = paste("Patient has vital status other than UNKNOWN (", vital_status, ") and vital status timestamp is not provided", sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_patients %>% dplyr::filter(!is.na(vital_status_timestamp) & vital_status_timestamp > Sys.Date()) %>% dplyr::mutate(warningType = "Vital status timestamp is in the future", warning = paste("Vital status timestamp ", vital_status_timestamp, sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_patients %>% dplyr::filter(!is.na(diag_date) & diag_date > Sys.Date()) %>% dplyr::mutate(warningType = "Initial diagnosis date is in the future", warning = paste("Initial diagnosis date ", diag_date, sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::filter(pharma_scheme == "Other" & is.na(pharma_scheme_other)) %>% dplyr::mutate(warningType = "Pharmacotherapy scheme description is missing while pharmacotherapy scheme is Other", warning = "") %>% dplyr::mutate(patientid = patient_id) %>% select(locationName, id, patientid, warningType, warning) 
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::mutate(pharma_scheme_other = gsub("\\s+", " ", str_trim(pharma_scheme_other))) %>% dplyr::filter(treatment_type == "pharma" & pharma_scheme == "Other" & pharma_scheme_other %in% c("No pharmacotherapy", "other", "unknown", "NULL")) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Suspicious description of pharmacotherapy", warning = paste("Suspicious value is '", pharma_scheme_other, "'", sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::mutate(pharma_scheme_other = gsub("\\s+", " ", str_trim(pharma_scheme_other))) %>% dplyr::filter(treatment_type == "pharma" & pharma_scheme == "Other" & pharma_scheme_other %in% c("neoadjuvante Radiochemo", "Substances: unbekannt")) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Missing specification of used substances in pharmacotherapy description", warning = paste("Suspicious value is '", pharma_scheme_other, "'", sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::mutate(pharma_scheme_other = gsub("\\s+", " ", str_trim(pharma_scheme_other))) %>% dplyr::filter(treatment_type == "pharma" & pharma_scheme == "Other" & (str_detect(pharma_scheme_other, "%-FU") | str_detect(pharma_scheme_other, "andLeucovorin") )) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Suspicious characters or words in description of pharmacotherapy", warning = paste("Suspicious value is '", pharma_scheme_other, "'", sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

#w <- rows_treatments %>% dplyr::filter(surgery_type == "Other" & is.na(surgery_type_other)) %>% dplyr::mutate(warningType = "Surgery type description is missing while surgery type is Other", warning = "") %>% dplyr::mutate(patientid = patient_id) %>% select(locationName, id, patientid, warningType, warning)
#CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::filter(treatment_type == "surgery") %>% dplyr::group_by(case_id) %>% dplyr::summarise(surgery_count = n()) %>% ungroup() %>% right_join(rows_treatments, by = "case_id") %>% dplyr::filter(treatment_type == "surgery" & surgery_count == 1) %>% select(id, case_id, patient_id, surgery_location, surgery_radicality, surgery_type, surgery_type_other, treatment_start) %>% dplyr::mutate(patientid = patient_id) %>% left_join(rows_patients, by = c("case_id" = "id")) %>% dplyr::filter(!is.na(surgery_location) & surgery_location != hist_loc) %>% dplyr::mutate(warningType = "Surgery and histological location do not match", warning = paste("Surgery location is ", surgery_location, " while histological location is ", hist_loc, sep="")) %>% dplyr::mutate(patientid = patientid.x) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::filter(treatment_type == "surgery") %>% dplyr::group_by(case_id) %>% dplyr::summarise(surgery_count = n()) %>% ungroup() %>% right_join(rows_treatments, by = "case_id") %>% dplyr::filter(treatment_type == "surgery" & surgery_count > 1) %>% select(id, case_id, patient_id, surgery_location, surgery_radicality, surgery_type, surgery_type_other, surgery_count, treatment_start) %>% dplyr::mutate(patientid = patient_id) %>% left_join(rows_patients, by = c("case_id" = "id")) %>% dplyr::filter(!is.na(surgery_location) & surgery_location != hist_loc) %>% dplyr::mutate(warningType = "Surgery and histological location do not match (but multiple surgeries per patient)", warning = paste0("Surgery location is ", surgery_location, " while histological location is ", hist_loc, ", total number of surgeries ", surgery_count)) %>% dplyr::mutate(patientid = patientid.x) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))


#w <- rows_treatments %>% dplyr::filter(treatment_type == "surgery") %>% select(id, case_id, patient_id, surgery_location, surgery_radicality, surgery_type, surgery_type_other, treatment_start) %>% dplyr::mutate(patientid = patient_id) %>% left_join(rows_patients, by = c("case_id" = "id")) %>% dplyr::filter(!is.na(surgery_location) & surgery_location != hist_loc) %>% dplyr::mutate(warningType = "Surgery and histological location do not match (soft check: maybe false positive)", warning = paste("Surgery location is ", surgery_location, " while histological location is ", hist_loc, sep="")) %>% dplyr::mutate(patientid = patientid.x) %>% select(locationName, id, patientid, warningType, warning)
#CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::filter(treatment_type == "surgery" & !is.na(surgery_location) & !is.na(surgery_type)) %>% select(id, case_id, patient_id, locationName, surgery_location, surgery_radicality, surgery_type, surgery_type_other, treatment_start) %>%  dplyr::mutate(patientid = patient_id, warning = Vectorize(getValidSurgeries)(surgery_location, surgery_type), warningType = "Mismatch between surgery location and surgery type") %>% dplyr::filter(!is.na(warning)) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

# the following 2 checks are redundant since Florian is already checking empty values

# w <- rows_responses %>% dplyr::filter(is.na(response_value)) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Missing value of response to therapy", warning = "") %>% select(locationName, id, patientid, warningType, warning)
# CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

# w <- rows_responses %>% dplyr::filter(is.na(response_start)) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Missing or invalid start of response to therapy", warning = "") %>% select(locationName, id, patientid, warningType, warning)
# CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_events %>% dplyr::filter(!is.na(event_end) & event_end < event_start) %>% dplyr::mutate(warningType = "Negative event (treatment/response) duration: end time is before start time", warning = paste("Start value is ", event_start, " weeks, end value is ", event_end, " weeks", sep=""), patientid = patient_id) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

# adjusted_overall_survival + 1 is to avoid simple week rounding problems
w <- rows_events %>% left_join(rows_patients, by = c("case_id" = "id")) %>% dplyr::mutate(locationName =locationName.x, vital_status=factor(vital_status)) %>% dplyr::filter(event_start > adjusted_overall_survival | (!is.na(event_end) & event_end > adjusted_overall_survival+1)) %>% dplyr::mutate(warningType = "Event (treatment/response) starts or ends after survival of patient", warning = paste("Start value is ", event_start, " weeks, end value is ", event_end, " weeks, adjusted overall survival is ", adjusted_overall_survival, " weeks", sep=""), patientid = patient_id) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))


w <- rows_responses %>% dplyr::filter(!is.na(response_start) & response_start < 0) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Start of response to therapy is before diagnosis", warning = paste("Start value is ", response_start, sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_responses %>% dplyr::mutate(response_start = as.numeric(response_start)) %>% dplyr::group_by(patient_id) %>% dplyr::filter(response_start == max(response_start)) %>% ungroup() %>% left_join(rows_patients, by = c("case_id" = "id")) %>% dplyr::mutate(locationName =locationName.x, vital_status=factor(vital_status)) %>% select(id, patientid, locationName, response_start, response_value, vital_status, adjusted_overall_survival) %>% dplyr::mutate(response_start = as.numeric(response_start), locationName = factor(locationName)) %>% dplyr::filter(vital_status == "DEATH_COLON_CANCER" & response_value == "Complete response") %>% dplyr::mutate(warningType = "Suspect incomplete followup: patient died of colon cancer while last response to therapy is Complete response", warning = paste("Last response starts at week ", response_start, ", adjusted overall surivival is ", adjusted_overall_survival, " weeks", sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_responses %>% select(id, case_id, patient_id, response_start) %>% dplyr::mutate(response_start = as.numeric(response_start)) %>% left_join(rows_patients, by = c("case_id" = "id")) %>% dplyr::filter(!is.na(diag_date) & diag_date + response_start*7 > Sys.Date()) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Start of response to therapy is in the future", warning = paste("Response start is ", response_start, " weeks, converted to absolute dates it is ", as.Date(diag_date + response_start*7), sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::filter(!is.na(treatment_start) & treatment_start < 0) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Start of therapy is before diagnosis", warning = paste("Start value is ", treatment_start, sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% select(id, case_id, patient_id, treatment_start) %>% dplyr::mutate(treatment_start = as.numeric(treatment_start)) %>% left_join(rows_patients, by = c("case_id" = "id")) %>% dplyr::filter(!is.na(diag_date) & diag_date + treatment_start*7 > Sys.Date()) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Start of treatment is in the future", warning = paste("Treatment start is ", treatment_start, " weeks, converted to absolute dates it is ", as.Date(diag_date + treatment_start*7), sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% select(id, case_id, patient_id, treatment_end) %>% dplyr::mutate(treatment_end = as.numeric(treatment_end)) %>% left_join(rows_patients, by = c("case_id" = "id")) %>% dplyr::filter(!is.na(diag_date) & diag_date + treatment_end*7 > Sys.Date()) %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "End of treatment is in the future", warning = paste("Treatment end is ", treatment_end, " weeks, converted to absolute dates it is ", as.Date(diag_date + treatment_end*7), sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_treatments %>% dplyr::filter(!is.na(treatment_start) & !is.na(treatment_end) & treatment_end == 0 & treatment_start == 0 & treatment_type != "surgery") %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Non-surgery therapy starts and ends in week 0 since initial diagnosis (maybe false positive)", warning = paste("Treatment type is ", treatment_type,ifelse(treatment_type == "pharma", paste0(" (pharma scheme: ", pharma_scheme, ifelse(!is.na(pharma_scheme) & pharma_scheme == "Other", paste0(", pharma scheme description: ", pharma_scheme_other), ""), ")"), ""), sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

# this test excludes resutls provided by the previous tests
w <- rows_treatments %>% dplyr::filter(!is.na(treatment_start) & !is.na(treatment_end) & treatment_end - treatment_start == 0 & treatment_start != 0 & treatment_type == "pharma") %>% dplyr::mutate(patientid = patient_id) %>% dplyr::mutate(warningType = "Suspiciously short pharma therapy - less than 1 week (may be false positive)", warning = paste("Start value is ", treatment_start, ", end value is ", treatment_end, ", pharma scheme: ", pharma_scheme, ifelse(!is.na(pharma_scheme) & pharma_scheme == "Other", paste0(", pharma scheme description: ", pharma_scheme_other), ""), sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))


w <- rows_patients %>% dplyr::filter(!is.na(tnm_pt) & !is.na(tnm_pn) & !is.na(tnm_pm) & !is.na(uicc_version)) %>% dplyr::mutate(uicc_stage_computed = Vectorize(getStage)(tnm_pt, tnm_pn, tnm_pm, uicc_version)) %>% select(locationName, id, patientid, tnm_pt, tnm_pn, tnm_pm, uicc_version, uicc_stage, uicc_stage_computed) %>% dplyr::filter(!is.na(uicc_stage_computed) & uicc_stage != uicc_stage_computed) %>% dplyr::mutate(warningType = "Mismatch between provided and computed stage value", warning = paste("Provided data: pT = ", tnm_pt, ", pN = ", tnm_pn, ", pM = ", tnm_pm, ", UICC version - ", uicc_version, ". Provided stage: ", uicc_stage, " Computed stage: ", uicc_stage_computed, sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_patients %>% dplyr::filter(!is.na(tnm_pt) & !is.na(tnm_pn) & !is.na(tnm_pm) & !is.na(uicc_version)) %>% dplyr::filter(uicc_version %in% c("6th edition", "7th edition") & tnm_pm != "MX" & tnm_pn != "NX") %>% dplyr::mutate(uicc_stage_computed = Vectorize(getStage)(tnm_pt, tnm_pn, tnm_pm, uicc_version)) %>% select(locationName, id, patientid, tnm_pt, tnm_pn, tnm_pm, uicc_version, uicc_stage, uicc_stage_computed) %>% dplyr::filter(is.na(uicc_stage_computed)) %>% dplyr::mutate(warningType = "Suspicious TNM value combination for given UICC version (e.g., N2a for UICC version 6) or uncomputable UICC stage", warning = paste("Provided data: pT = ", tnm_pt, ", pN = ", tnm_pn, ", pM = ", tnm_pm, ", UICC version - ", uicc_version, ". Provided stage: ", uicc_stage, sep="")) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

w <- rows_patients %>% dplyr::filter(!is.na(tnm_pt) & !is.na(tnm_pn) & !is.na(tnm_pm) & !is.na(uicc_version)) %>% dplyr::filter(tnm_pn == "NX" & !is.na(uicc_stage) & !(uicc_stage %in% c("IV", "IVA", "IVB"))) %>% select(locationName, id, patientid, tnm_pt, tnm_pn, tnm_pm, uicc_version, uicc_stage) %>% dplyr::mutate(warningType = "pNX provided in TNM values, while UICC stage is determined (how?)", warning = paste0("Provided data: pT = ", tnm_pt, ", pN = ", tnm_pn, ", pM = ", tnm_pm, ", provided stage: ", uicc_stage)) %>% select(locationName, id, patientid, warningType, warning)
CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))


#w <- rows_patients %>% dplyr::filter(!is.na(tnm_pt) & !is.na(tnm_pn) & !is.na(tnm_pm) & !is.na(uicc_version)) %>% dplyr::filter(uicc_version %in% c("6th edition") & !is.na(diag_date) & ( as.numeric(format(diag_date, "%Y")) < 2003 | as.numeric(format(diag_date, "%Y")) > 2010 )) %>% dplyr::mutate(warningType = "UICC version used outside of its normal years", warning = paste("UICC version - ", uicc_version, " should be used 2003-2010. Year of diagnosis = ", format(diag_date, "%Y"), sep="")) %>% select(locationName, id, patientid, warningType, warning)
#CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

#w <- rows_patients %>% dplyr::filter(!is.na(tnm_pt) & !is.na(tnm_pn) & !is.na(tnm_pm) & !is.na(uicc_version)) %>% dplyr::filter(uicc_version %in% c("7th edition") & !is.na(diag_date) & ( as.numeric(format(diag_date, "%Y")) < 2010 | as.numeric(format(diag_date, "%Y")) > 2017 )) %>% dplyr::mutate(warningType = "UICC version used outside of its normal years", warning = paste("UICC version - ", uicc_version, " should be used 2010-2017. Year of diagnosis = ", format(diag_date, "%Y"), sep="")) %>% select(locationName, id, patientid, warningType, warning)
#CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

#w <- rows_patients %>% dplyr::filter(!is.na(tnm_pt) & !is.na(tnm_pn) & !is.na(tnm_pm) & !is.na(uicc_version)) %>% dplyr::filter(uicc_version %in% c("5th edition") & !is.na(diag_date) & ( as.numeric(format(diag_date, "%Y")) < 1997 | as.numeric(format(diag_date, "%Y")) > 2003 )) %>% dplyr::mutate(warningType = "UICC version used outside of its normal years", warning = paste("UICC version - ", uicc_version, " should be used 1997-2003 Year of diagnosis = ", format(diag_date, "%Y"), sep="")) %>% select(locationName, id, patientid, warningType, warning)
#CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

#w <- rows_patients %>% dplyr::filter(!is.na(tnm_pt) & !is.na(tnm_pn) & !is.na(tnm_pm) & !is.na(uicc_version)) %>% dplyr::filter(uicc_version %in% c("8th edition") & !is.na(diag_date) & ( as.numeric(format(diag_date, "%Y")) < 2017)) %>% dplyr::mutate(warningType = "UICC version used outside of its normal years", warning = paste("UICC version - ", uicc_version, " should be used 2017 onward. Year of diagnosis = ", format(diag_date, "%Y"), sep="")) %>% select(locationName, id, patientid, warningType, warning)
#CRCwarnings <- CRCwarnings %>% full_join(w, by = c("locationName", "id", "patientid", "warningType", "warning"))

CRCwarnings %>% write.xlsx("reported-errors.xlsx")
