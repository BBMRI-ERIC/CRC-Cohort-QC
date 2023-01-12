library(foreach)
library(DBI)
library(RPostgreSQL)
library(tidyverse)
library(dbplyr)
library(reshape2)
library(xlsx)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="dbname", user="username", password="password", host="127.0.0.1")
rs <- dbSendQuery(con,"SELECT distinct patients.id, 
  caseForm.data ->> 'urn:ccdg:dataelement:51:3' AS diag_date,
  caseForm.data ->> 'urn:ccdg:dataelement:3:1' AS age_at_primary_diagnosis,
  caseForm.data ->> 'urn:ccdg:dataelement:5:2' AS vital_status,
  caseForm.data ->> 'urn:ccdg:dataelement:6:3' AS vital_status_timestamp,
  caseForm.data ->> 'urn:ccdg:dataelement:85:1' AS sex,
  caseForm.data ->> 'urn:ccdg:dataelement:7:2' AS overall_survival,
  episodeForms.data ->> 'urn:ccdg:dataelement:71:1' AS tnm_pt,
  episodeForms.data ->> 'urn:ccdg:dataelement:77:1' AS tnm_pn,
  episodeForms.data ->> 'urn:ccdg:dataelement:75:1' AS tnm_pm,
  episodeForms.data ->> 'urn:ccdg:dataelement:91:1' AS hist_morpho,
  episodeForms.data ->> 'urn:ccdg:dataelement:92:1' AS hist_loc,
  episodeForms.data ->> 'urn:ccdg:dataelement:70:2' AS uicc_stage, 
  episodeForms.data ->> 'urn:ccdg:dataelement:73:3' AS uicc_version, 
  episodeForms.data ->> 'urn:ccdg:dataelement:83:1' AS who_grade, 
  episodeForms.data ->> 'urn:ccdg:dataelement:58:2' AS dig_img_invasion_front, 
  episodeForms.data ->> 'urn:ccdg:dataelement:57:3' AS dig_img_avail,
  episodeForms.data ->> 'urn:ccdg:dataelement:89:3' AS sample_year,
  caseForm.data ->> 'urn:ccdg:dataelement:14:3' AS mm_msi,
  caseForm.data ->> 'urn:ccdg:dataelement:15:2' AS mm_mrge,
  caseForm.data ->> 'urn:ccdg:dataelement:16:3' AS mm_risk_hnpcc,
  caseForm.data ->> 'urn:ccdg:dataelement:20:3' AS mm_kras_ex2,
  caseForm.data ->> 'urn:ccdg:dataelement:21:5' AS mm_kras_ex3,
  caseForm.data ->> 'urn:ccdg:dataelement:22:4' AS mm_kras_ex4,
  caseForm.data ->> 'urn:ccdg:dataelement:23:5' AS mm_nras_ex2,
  caseForm.data ->> 'urn:ccdg:dataelement:24:4' AS mm_nras_ex3,
  caseForm.data ->> 'urn:ccdg:dataelement:25:3' AS mm_nras_ex4,
  patients.data ->> 'patientID' AS patientID,
  Locations.name as \"locationName\"
  FROM public.\"caseForms\" as caseForm,
  (Select id, location_id, patient_id from public.cases) as cases,
  (Select id, data from public.patients) as patients, 
  (Select id, episode_id, data, name from public.\"episodeForms\") as episodeForms,
  (Select id, case_id from public.episodes) as episodes,
  (Select id, data, name from locations) as locations
  where caseForm.case_id = cases.id and patients.id = cases.patient_id and cases.id = episodes.case_id and episodeForms.episode_id = episodes.id and locations.id = cases.location_id and episodeForms.name = 'form_34_ver-22'
  order by patients.id;
")
#  where caseForm.case_id = cases.id and patients.id = cases.patient_id and cases.id = episodes.case_id and episodeForms.episode_id = episodes.id and locations.id = cases.location_id and (episodeForms.data ->> 'urn:ccdg:dataelement:70:2') IS NOT NULL

rows_patients <- as_tibble(fetch(rs, n=-1)) %>% dplyr::filter(locationName != "Example Location")  %>% dplyr::mutate(overall_survival = as.numeric(overall_survival)) %>% dplyr::mutate(tnm_pt = gsub("Primary Tumor - ", "", tnm_pt), tnm_pn = gsub("Regional lymph nodes - ", "", tnm_pn), tnm_pm = gsub("Distant metastasis - ", "", tnm_pm), hist_loc = gsub("Localization of primary tumor - ", "", hist_loc), uicc_stage = gsub("Stage - ", "", uicc_stage), uicc_stage = gsub("II A", "IIA", uicc_stage), who_grade = gsub("WHO Grading - Grade - ", "", who_grade), age_at_primary_diagnosis = as.numeric(age_at_primary_diagnosis), uicc_version = factor(uicc_version)) %>% dplyr::mutate(diag_date = as.Date(diag_date), vital_status_timestamp = as.Date(vital_status_timestamp), fromDiagToVitalCheckWeeks = as.numeric(vital_status_timestamp - diag_date)/7, survivalToVitalRatio = overall_survival/fromDiagToVitalCheckWeeks) %>% dplyr::mutate(reportedPatientID = paste(id, " (", patientid, ")", sep=""))

rows_institutions <- rows_patients %>% group_by(locationName) %>% count()

rs <- dbSendQuery(con,"SELECT distinct patients.id, 
  episodeForms.data ->> 'urn:ccdg:dataelement:89:3' AS sample_year,
  episodeForms.data ->> 'urn:ccdg:dataelement:56:2' AS sample_id,
  episodeForms.data ->> 'urn:ccdg:dataelement:55:2' AS sample_material,
  episodeForms.data ->> 'urn:ccdg:dataelement:54:2' AS sample_type,
  patients.data ->> 'patientID' AS patient_id,
  caseForm.case_id AS case_id,
  Locations.name as \"locationName\"
  FROM public.\"caseForms\" as caseForm,
  (Select id, location_id, patient_id from public.cases) as cases,
  (Select id, data from public.patients) as patients, 
  (Select id, episode_id, data, name from public.\"episodeForms\") as episodeForms,
  (Select id, case_id from public.episodes) as episodes,
  (Select id, data, name from locations) as locations
  where caseForm.case_id = cases.id and patients.id = cases.patient_id and cases.id = episodes.case_id and episodeForms.episode_id = episodes.id and locations.id = cases.location_id and episodeForms.name = 'form_35_ver-6'
  order by patients.id;
")
rows_samples <- as_tibble(fetch(rs, n=-1)) %>% dplyr::filter(locationName != "Example Location")  %>% mutate(sample_year_num = as.numeric(sample_year))

rs <- dbSendQuery(con,"SELECT distinct patients.id, 
  episodeForms.name AS event_treatment_form,
  episodeForms.data ->> 'urn:ccdg:dataelement:12:4' AS radiation_start,
  episodeForms.data ->> 'urn:ccdg:dataelement:13:2' AS radiation_end,
  episodeForms.data ->> 'urn:ccdg:dataelement:35:3' AS targeted_start,
  episodeForms.data ->> 'urn:ccdg:dataelement:36:1' AS targeted_end,
  episodeForms.data ->> 'urn:ccdg:dataelement:34:1' AS response_start,
  episodeForms.data ->> 'urn:ccdg:dataelement:33:1' AS response_value,
  episodeForms.data ->> 'urn:ccdg:dataelement:8:3' AS surgery_start,
  episodeForms.data ->> 'urn:ccdg:dataelement:93:1' AS surgery_location,
  episodeForms.data ->> 'urn:ccdg:dataelement:9:2' AS surgery_radicality,
  episodeForms.data ->> 'urn:ccdg:dataelement:49:1' AS surgery_type,
  episodeForms.data ->> 'urn:ccdg:dataelement:67:1' AS surgery_type_other,
  episodeForms.data ->> 'urn:ccdg:dataelement:10:2' AS pharma_start,
  episodeForms.data ->> 'urn:ccdg:dataelement:11:2' AS pharma_end,
  episodeForms.data ->> 'urn:ccdg:dataelement:59:5' AS pharma_scheme,
  episodeForms.data ->> 'urn:ccdg:dataelement:81:3' AS pharma_scheme_other,
  patients.data ->> 'patientID' AS patient_id,
  caseForm.case_id AS case_id,
  Locations.name as \"locationName\"
  FROM public.\"caseForms\" as caseForm,
  (Select id, location_id, patient_id from public.cases) as cases,
  (Select id, data from public.patients) as patients, 
  (Select id, episode_id, data, name from public.\"episodeForms\") as episodeForms,
  (Select id, case_id from public.episodes) as episodes,
  (Select id, data, name from locations) as locations
  where caseForm.case_id = cases.id and patients.id = cases.patient_id and cases.id = episodes.case_id AND episodeForms.episode_id = episodes.id AND locations.id = cases.location_id AND (episodeForms.name = 'form_29_ver-5' OR episodeForms.name = 'form_30_ver-3' OR episodeForms.name = 'form_31_ver-2' OR episodeForms.name = 'form_32_ver-8' OR episodeForms.name = 'form_33_ver-10')
  order by patients.id;
")
rows_events <- as_tibble(fetch(rs, n=-1)) %>% dplyr::filter(locationName != "Example Location") %>%
mutate(event_start = as.numeric(case_when(
event_treatment_form == "form_29_ver-5" ~ radiation_start,
event_treatment_form == "form_30_ver-3" ~ targeted_start,
event_treatment_form == "form_32_ver-8" ~ surgery_start,
event_treatment_form == "form_33_ver-10" ~ pharma_start,
event_treatment_form == "form_31_ver-2" ~ response_start
)),
event_end = as.numeric(case_when(
event_treatment_form == "form_29_ver-5" ~ radiation_end,
event_treatment_form == "form_30_ver-3" ~ targeted_end,
event_treatment_form == "form_33_ver-10" ~ pharma_end
)))

rows_treatments <- rows_events %>% filter(event_treatment_form == "form_29_ver-5" | event_treatment_form == "form_30_ver-3" | event_treatment_form == "form_32_ver-8" | event_treatment_form == "form_33_ver-10")  %>% 
mutate(treatment_start = as.numeric(case_when (
event_treatment_form == "form_29_ver-5" ~ radiation_start, 
event_treatment_form == "form_30_ver-3" ~ targeted_start,
event_treatment_form == "form_32_ver-8" ~ surgery_start,
event_treatment_form == "form_33_ver-10" ~ pharma_start
))) %>% 
mutate(treatment_end = as.numeric(case_when (
event_treatment_form == "form_29_ver-5" ~ radiation_end, 
event_treatment_form == "form_30_ver-3" ~ targeted_end,
event_treatment_form == "form_33_ver-10" ~ pharma_end
))) %>% 
dplyr::mutate(treatment_type = factor(case_when (
event_treatment_form == "form_29_ver-5" ~ "radiation",
event_treatment_form == "form_30_ver-3" ~ "targeted",
event_treatment_form == "form_32_ver-8" ~ "surgery",
event_treatment_form == "form_33_ver-10" ~ "pharma",
))) %>% 
select(-response_start, -response_value) %>% 
dplyr::mutate(surgery_location = factor(gsub("Location of tumor - ", "", surgery_location))) %>%
dplyr::mutate(pharma_scheme = factor(pharma_scheme), surgery_radicality = factor(surgery_radicality), surgery_type = factor(surgery_type))

# this is to add first treatment start and adjusted overall survival (i.e., overall survival plus first treatment start if it is defined)
rows_patients <- rows_treatments %>% dplyr::group_by(patient_id) %>% dplyr::mutate(first_treatment_start = min(treatment_start)) %>% dplyr::ungroup() %>% select(id, first_treatment_start) %>% unique() %>% right_join(rows_patients) %>% dplyr::mutate(adjusted_overall_survival = ifelse(is.na(first_treatment_start), overall_survival, overall_survival + first_treatment_start)) 
 
rows_responses <- rows_events %>% filter(event_treatment_form == "form_31_ver-2") %>% select(-event_treatment_form) %>% dplyr::mutate(response_value = factor(gsub("Specific response - ", "", response_value))) %>% select(id, patient_id, case_id, locationName, response_start, response_value)

dbDisconnect(con)

options(tibble.width = Inf)

