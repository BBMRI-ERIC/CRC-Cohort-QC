#CCDC request with functions

library(foreach)
library(DBI)
library(RPostgreSQL)
library(tidyverse)
library(dbplyr)
library(reshape2)
library(plyr)
library(ggplot2)
library(xlsx)


# helper functions

getDBConnection <- function() {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="db", user="username", password="password", host="127.0.0.1")
  
  #on.exit(dbDisconnect(con))
  
  return(con)
}

# plot drawing/early draft implementation

createPlotWithSampleYears <- function(con) {
  #sample year check
  
  rs <- dbSendQuery(con,"select locations.\"name\" as location_name, count (patients.id) as patient_count
                  from \"episodeForms\"
                  
                  left join episodes on \"episodeForms\".episode_id = episodes.id 
                  left join cases on public.cases.id = episodes.case_id
                  left join locations on locations.id = cases.location_id
                  left join patients on patients.id = cases.patient_id
                  
                  
                  where \"episodeForms\".name like 'form_35_ver-6' 
                  and \"episodeForms\".data ->> 'urn:ccdg:dataelement:89:3' is not null
                  
                  
                  group by location_name;")
  
  patients_with_year_samples_count <- as_tibble(fetch(rs, n=-1)) %>% mutate(patient_count = as.numeric(patient_count))
  
  ggplot(data=patients_with_year_samples_count,  aes(x=location_name, y=patient_count))+ ggtitle("Patients with sample year value") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

}

createPlotWithoutSampleYears <- function(con){
  #inverted sample year check
  
  rs <- dbSendQuery(con,"select locations.\"name\" as location_name, count (patients.id) as patient_count
                  from \"episodeForms\"
                  
                  left join episodes on \"episodeForms\".episode_id = episodes.id 
                  left join cases on public.cases.id = episodes.case_id
                  left join locations on locations.id = cases.location_id
                  left join patients on patients.id = cases.patient_id
                  
                  
                  where \"episodeForms\".name like 'form_35_ver-6' 
                  and \"episodeForms\".data ->> 'urn:ccdg:dataelement:89:3' is null
                  
                  
                  group by location_name;")
  
  
  
  
  patients_without_year_samples_count <- as_tibble(fetch(rs, n=-1)) %>% mutate(patient_count = as.numeric(patient_count))
  
  
  ggplot(data=patients_without_year_samples_count,  aes(x=location_name, y=patient_count))+ ggtitle("Patients without sample year value") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}

createPlotWithoutSampleID <- function(con){
  # patients without sampleID
  
  rs <- dbSendQuery(con,"select locations.id as location_id, episodes.case_id as case_id, patients.\"data\" ->> 'patientID', patients.id, locations.\"name\" as location_name, \"episodeForms\".data ->> 'urn:ccdg:dataelement:56:2' as \"Year of Sample\", \"episodeForms\".data from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join locations on locations.id = cases.location_id
                    left join patients on patients.id = cases.patient_id
                    
                    
                    where \"episodeForms\".name like 'form_35_ver-6' 
                    and \"episodeForms\".data ->> 'urn:ccdg:dataelement:56:2' is null
                    
                    order by \"Year of Sample\" desc;")
  
  
  missing_sample_ids <- as_tibble(fetch(rs, n=-1))
  
  ggplot(data=missing_sample_ids,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without sample_id") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
}

createPlotWithoutPreservationMode <- function(con){
  # patients without preservation mode
  
  rs <- dbSendQuery(con,"select locations.id as location_id, episodes.case_id as case_id, locations.\"name\" as location_name, patients.\"data\" ->> 'patientID', patients.id, \"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' as preservation_mode from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    
                    where \"episodeForms\".name like 'form_35_ver-6' 
                    and \"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' is null;")
  
  
  missing_preservation_mode <- as_tibble(fetch(rs, n=-1))
  
  ggplot(data=missing_preservation_mode,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without sample_id") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}

createPlotWithoutMaterialType<- function(con){
  # patients without material type
  
  rs <- dbSendQuery(con,"select locations.\"name\" as location_name, patients.\"data\" ->> 'patientID', patients.id, \"episodeForms\".data ->> 'urn:ccdg:dataelement:54:2' as \"Material type\", locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
left join episodes on \"episodeForms\".episode_id = episodes.id 
                  left join cases on public.cases.id = episodes.case_id
                  left join patients on patients.id = cases.patient_id
                  left join locations on locations.id = cases.location_id
                  
                  
                  where \"episodeForms\".name like 'form_35_ver-6' 
                  and \"episodeForms\".data ->> 'urn:ccdg:dataelement:54:2' is null;")
  
  
  missing_material_type <- as_tibble(fetch(rs, n=-1))
  
  ggplot(data=missing_material_type,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without sample_id") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}

createPlotsWithoutHistoValues<- function(con){

  #Histo
  
  rs <- dbSendQuery(con,"select locations.id as locations_id, locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:68:2' as Localization_of_metastasis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:92:1' as Localization_of_primary_tumor,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:91:1' as Morphology,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:83:1' as Grade,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:77:1'  as Regional_lymph_nodes,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:71:1'  as Primary_Tumor,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:75:1'  as Distant_metastasis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:73:3'  as UICC_version,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:70:2'  as UICC_Stage,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:68:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:92:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:91:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:83:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:77:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:71:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:75:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:73:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:70:2' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_34%' 
                    
                    order by patients.id;")
  
  histo <- as_tibble(fetch(rs, n=-1))
  histo_localization_of_metastasis <- histo[,c("location_id", "localization_of_metastasis", "location_name", "patient_id", "patient_name")] %>% filter(is.na(localization_of_metastasis))
  histo_localization_of_primary_tumor <- histo[,c("location_id", "localization_of_primary_tumor", "location_name", "patient_id", "patient_name")] %>% filter(is.na(localization_of_primary_tumor))
  histo_morphology <- histo[,c("location_id", "morphology", "location_name", "patient_id", "patient_name")] %>% filter(is.na(morphology))
  histo_grade<- histo[,c("location_id", "grade", "location_name", "patient_id", "patient_name")] %>% filter(is.na(grade))
  histo_regional_lymph_nodes<- histo[,c("location_id", "regional_lymph_nodes", "location_name", "patient_id", "patient_name")] %>% filter(is.na(regional_lymph_nodes))
  histo_primary_Tumor<- histo[,c("location_id", "primary_tumor", "location_name", "patient_id", "patient_name")] %>% filter(is.na(primary_tumor))
  histo_distant_metastasis<- histo[,c("location_id", "distant_metastasis", "location_name", "patient_id", "patient_name")] %>% filter(is.na(distant_metastasis))
  histo_uicc_version<- histo[,c("location_id", "uicc_version", "location_name", "patient_id", "patient_name")] %>% filter(is.na(uicc_version))
  histo_uicc_stage<- histo[,c("location_id", "uicc_stage", "location_name", "patient_id", "patient_name")] %>% filter(is.na(uicc_stage))
  
  
  
  ggplot(data=histo_localization_of_metastasis,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without localization of metasis") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=histo_localization_of_primary_tumor,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without localization of primary tumor") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=histo_morphology,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without histo_morphology") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=histo_grade,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without histo_grade") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=histo_regional_lymph_nodes,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without histo_regional_lymph_nodes") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=histo_primary_Tumor,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without histo_primary_Tumor") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=histo_distant_metastasis,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without distant_metastasisr") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=histo_uicc_version,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without histo_uicc_version") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=histo_uicc_stage,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("Patients without histo_uicc_stage") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
}

createPlotsWithoutSurgeryValues<- function(con){

  #Surgery
  
  rs <- dbSendQuery(con,"
select patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, locations.name as location_name,
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:8:3' as time_difference_between_initial_diagnosis_and_surger,
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:9:2' as surgery_radicality,
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:49:1' as surgery_type,
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:93:1' as location_of_the_tumor,
                  
                  \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                  left join episodes on \"episodeForms\".episode_id = episodes.id 
                  left join cases on public.cases.id = episodes.case_id
                  left join patients on patients.id = cases.patient_id
                  left join locations on locations.id = cases.location_id
                  
                  where 
                  (
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:8:3' is null
                  or
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:9:2' is null
                  or
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:49:1' is null
                  or
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:93:1' is null
                  )
                  
                  and 
                  
                  \"episodeForms\".name like '%form_32%' 
                  
                  order by patients.id;")
  
  surgery <- as_tibble(fetch(rs, n=-1))
  surgey_time_difference_between_initial_diagnosis_and_surger <- surgery[,c("location_id", "time_difference_between_initial_diagnosis_and_surger", "location_name", "patient_id", "patient_name")] %>% filter(is.na(time_difference_between_initial_diagnosis_and_surger))
  surgey_surgery_radicality <- surgery[,c("location_id", "surgery_radicality", "location_name", "patient_id", "patient_name")] %>% filter(is.na(surgery_radicality))
  surgey_surgery_type <- surgery[,c("location_id", "surgery_type", "location_name", "patient_id", "patient_name")] %>% filter(is.na(surgery_type))
  surgey_location_of_the_tumor <- surgery[,c("location_id", "location_of_the_tumor", "location_name", "patient_id", "patient_name")] %>% filter(is.na(location_of_the_tumor))
  
  ggplot(data=surgey_time_difference_between_initial_diagnosis_and_surger,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("surgey_time_difference_between_initial_diagnosis_and_surger missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=surgey_surgery_radicality,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("surgey_surgery_radicality missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=surgey_surgery_type,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("surgey_surgery_type missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=surgey_location_of_the_tumor,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("surgey_location_of_the_tumor missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}

createPlotsWithoutPatientValues<- function(con){

  #Patient
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:3:1' as Age_at_diagnosis_rounded_to_years,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:85:1' as Biological_sex,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:5:2' as Vital_status,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:7:2' as Overall_survival_status,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:88:1' as Colonoscopy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:31:3' as CT,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:61:5' as Liver_imaging,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:63:4' as Lung_imaging,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:30:3' as MRI,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:15:2' as Mismatch_repair_gene_expression,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:14:3' as Microsatellite_instability,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:20:3' as KRAS_exon_2_codons_12_or_13,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:21:5' as KRAS_exon_3_codons_59_or_61,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:25:3' as KRAS_exon_4_codons_117_or_146_mutation_status,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:3:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:85:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:5:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:7:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:88:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:31:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:61:5' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:63:4' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:30:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:15:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:14:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:20:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:21:5' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:25:3' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_28%' 
                    
                    order by patients.id;")
  
  patient <- as_tibble(fetch(rs, n=-1))
  patient_age_at_diagnosis_rounded_to_years <- patient[,c("location_id", "age_at_diagnosis_rounded_to_years", "location_name", "patient_id", "patient_name")] %>% filter(is.na(age_at_diagnosis_rounded_to_years))
  patient_biological_sex <- patient[,c("location_id", "biological_sex", "location_name", "patient_id", "patient_name")] %>% filter(is.na(biological_sex))
  patient_vital_status <- patient[,c("location_id", "vital_status", "location_name", "patient_id", "patient_name")] %>% filter(is.na(vital_status))
  patient_overall_survival_status <- patient[,c("location_id", "overall_survival_status", "location_name", "patient_id", "patient_name")] %>% filter(is.na(overall_survival_status))
  patient_colonoscopy <- patient[,c("location_id", "colonoscopy", "location_name", "patient_id", "patient_name")] %>% filter(is.na(colonoscopy))
  patient_ct <- patient[,c("location_id", "ct", "location_name", "patient_id", "patient_name")] %>% filter(is.na(ct))
  patient_liver_imaging <- patient[,c("location_id", "liver_imaging", "location_name", "patient_id", "patient_name")] %>% filter(is.na(liver_imaging))
  patient_lung_imaging <- patient[,c("location_id", "lung_imaging", "location_name", "patient_id", "patient_name")] %>% filter(is.na(lung_imaging))
  patient_mri <- patient[,c("location_id", "mri", "location_name", "patient_id", "patient_name")] %>% filter(is.na(mri))
  patient_mismatch_repair_gene_expression <- patient[,c("location_id", "mismatch_repair_gene_expression", "location_name", "patient_id", "patient_name")] %>% filter(is.na(mismatch_repair_gene_expression))
  patient_microsatellite_instability <- patient[,c("location_id", "microsatellite_instability", "location_name", "patient_id", "patient_name")] %>% filter(is.na(microsatellite_instability))
  patient_kras_exon_2_codons_12_or_13 <- patient[,c("location_id", "kras_exon_2_codons_12_or_13", "location_name", "patient_id", "patient_name")] %>% filter(is.na(kras_exon_2_codons_12_or_13))
  patient_kras_exon_3_codons_59_or_61 <- patient[,c("location_id", "kras_exon_3_codons_59_or_61", "location_name", "patient_id", "patient_name")] %>% filter(is.na(kras_exon_3_codons_59_or_61))
  patient_kras_exon_4_codons_117_or_146_mutation_status <- patient[,c("location_id", "kras_exon_4_codons_117_or_146_mutation_status", "location_name", "patient_id", "patient_name")] %>% filter(is.na(kras_exon_4_codons_117_or_146_mutation_status))
  
  ggplot(data=patient_age_at_diagnosis_rounded_to_years,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_age_at_diagnosis_rounded_to_years missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_biological_sex,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_biological_sex missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_vital_status,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_vital_status missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_overall_survival_status,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_overall_survival_status missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_colonoscopy,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_colonoscopy missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_ct,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_ct missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_liver_imaging,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_liver_imaging missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_lung_imaging,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_lung_imaging missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_mri,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_mri missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_mismatch_repair_gene_expression,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_mismatch_repair_gene_expression missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_microsatellite_instability,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_age_at_diagnosis_rounded_to_years missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_kras_exon_2_codons_12_or_13,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_kras_exon_2_codons_12_or_13 missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_kras_exon_3_codons_59_or_61,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_kras_exon_3_codons_59_or_61 missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=patient_kras_exon_4_codons_117_or_146_mutation_status,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("patient_kras_exon_4_codons_117_or_146_mutation_status missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
}

createPlotsWithoutTargetedTherapy<- function(con){

  # targeted therapy
  
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,  
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:35:3' as date_of_start_of_targeted_therapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:36:1' as time_of_end_in_weeks_since_initial_diagnosis,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:35:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:36:1' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_30%' 
                    
                    order by patients.id;")
  
  targeted_therapy <- as_tibble(fetch(rs, n=-1))
  targeted_therapy_date_of_start_of_targeted_therapy <- targeted_therapy[,c("location_id", "date_of_start_of_targeted_therapy", "location_name", "patient_id", "patient_name")] %>% filter(is.na(date_of_start_of_targeted_therapy))
  targeted_therapy_time_of_end_in_weeks_since_initial_diagnosis <- targeted_therapy[,c("location_id", "time_of_end_in_weeks_since_initial_diagnosis", "location_name", "patient_id", "patient_name")] %>% filter(is.na(time_of_end_in_weeks_since_initial_diagnosis))
  
  ggplot(data=targeted_therapy_date_of_start_of_targeted_therapy,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("targeted_therapy_date_of_start_of_targeted_therapy missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=targeted_therapy_time_of_end_in_weeks_since_initial_diagnosis,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("targeted_therapy_time_of_end_in_weeks_since_initial_diagnosis missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
}
  
createPlotsWithoutPharmacotherapy<- function(con){

  # Pharmacotherapy
  
  
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,  
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:10:2' as Date_of_start_of_pharamacotherapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:11:2' as Date_of_end_of_pharamcotherapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:59:5' as Scheme_of_pharmacotherapy,
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:10:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:11:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:59:5' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_33%' 
                    
                    order by patients.id
                    ;")
  
  pharmacotherapy <- as_tibble(fetch(rs, n=-1))
  pharmacotherapy_date_of_start_of_pharamacotherapy <- pharmacotherapy[,c("location_id", "date_of_start_of_pharamacotherapy", "location_name", "patient_id", "patient_name")] %>% filter(is.na(date_of_start_of_pharamacotherapy))
  pharmacotherapy_date_of_end_of_pharamcotherapy <- pharmacotherapy[,c("location_id", "date_of_end_of_pharamcotherapy", "location_name", "patient_id", "patient_name")] %>% filter(is.na(date_of_end_of_pharamcotherapy))
  pharmacotherapy_scheme_of_pharmacotherapy <- pharmacotherapy[,c("location_id", "scheme_of_pharmacotherapy", "location_name", "patient_id", "patient_name")] %>% filter(is.na(scheme_of_pharmacotherapy))
  
  ggplot(data=pharmacotherapy_date_of_start_of_pharamacotherapy,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("pharmacotherapy_date_of_start_of_pharamacotherapy missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=pharmacotherapy_date_of_end_of_pharamcotherapy,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("pharmacotherapy_date_of_end_of_pharamcotherapy missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=pharmacotherapy_scheme_of_pharmacotherapy,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("pharmacotherapy_scheme_of_pharmacotherapy missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  }

createPlotsWithoutRadiationTherapy<- function(con){
  
  # Radiation Therapy
  
  
  rs <- dbSendQuery(con,"
                  select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
\"episodeForms\".data ->> 'urn:ccdg:dataelement:12:4' as Start_of_the_radiation_therapy_in_weeks_since_diagnosis,
\"episodeForms\".data ->> 'urn:ccdg:dataelement:13:2' as End_of_the_radiation_therapy_in_weeks_since_diagnosis,
\"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
left join episodes on \"episodeForms\".episode_id = episodes.id 
left join cases on public.cases.id = episodes.case_id
left join patients on patients.id = cases.patient_id
left join locations on locations.id = cases.location_id

where 
(
\"episodeForms\".data ->> 'urn:ccdg:dataelement:12:4' is null
or
\"episodeForms\".data ->> 'urn:ccdg:dataelement:13:2' is null
)

and 

\"episodeForms\".name like '%form_29%' 

order by patients.id
                  ;")
  
  radiationtherapy <- as_tibble(fetch(rs, n=-1))
  radiationtherapy_start_of_the_radiation_therapy_in_weeks_since_diagnosis <- radiationtherapy[,c("location_id", "start_of_the_radiation_therapy_in_weeks_since_diagnosis", "location_name", "patient_id", "patient_name")] %>% filter(is.na(start_of_the_radiation_therapy_in_weeks_since_diagnosis))
  radiationtherapy_end_of_the_radiation_therapy_in_weeks_since_diagnosis <- radiationtherapy[,c("location_id", "end_of_the_radiation_therapy_in_weeks_since_diagnosis", "location_name", "patient_id", "patient_name")] %>% filter(is.na(end_of_the_radiation_therapy_in_weeks_since_diagnosis))
  
  ggplot(data=radiationtherapy_start_of_the_radiation_therapy_in_weeks_since_diagnosis,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("radiationtherapy_start_of_the_radiation_therapy_in_weeks_since_diagnosis missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=radiationtherapy_end_of_the_radiation_therapy_in_weeks_since_diagnosis,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("radiationtherapy_end_of_the_radiation_therapy_in_weeks_since_diagnosis missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
}

createPlotsWithoutResponseToTherapy<- function(con){
  # Response to therapy
  
  rs <- dbSendQuery(con,"select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:34:1' as date_response_was_obtained_in_weeks_since_initial_diagnosis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:33:1' as specific_response,
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:34:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:33:1' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_31%' 
                    
                    order by patients.id
                    ;")
  
  
  response_to_therapy <- as_tibble(fetch(rs, n=-1))
  response_to_therapy_date_response_was_obtained_in_weeks_since_initial_diagnosis <- response_to_therapy[,c("location_id", "date_response_was_obtained_in_weeks_since_initial_diagnosis", "location_name", "patient_id", "patient_name")] %>% filter(is.na(date_response_was_obtained_in_weeks_since_initial_diagnosis))
  response_to_therapy_specific_response <- response_to_therapy[,c("location_id", "specific_response", "location_name", "patient_id", "patient_name")] %>% filter(is.na(specific_response))
  
  ggplot(data=response_to_therapy_date_response_was_obtained_in_weeks_since_initial_diagnosis,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("response_to_therapy_date_response_was_obtained_in_weeks_since_initial_diagnosis missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(data=response_to_therapy_specific_response,  aes(x=location_name, y=as.integer(as.logical(location_id))))+ ggtitle("response_to_therapy_specific_response missing") + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}

createPlotForAllMissedValues <- function(con, countWithMissingValuesPerBioBank, filtername, filter, title){
  #print("createPlotAllMissedValue")
  #print(filter)
  #ggplot(countWithMissingValuesPerBioBank,aes(x = location_name,y = value)) + ggtitle(title) +  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_log10() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(ggplot(countWithMissingValuesPerBioBank[countWithMissingValuesPerBioBank$location_name==filter,],aes(x = location_name,y = value)) + ggtitle(title) +  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_log10() + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
  ggplot(countWithMissingValuesPerBioBank[countWithMissingValuesPerBioBank$location_name==filter,],aes(x = location_name,y = value)) + ggtitle(title) +  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_log10() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  title <- gsub(" ", "_", title)
  title_pdf <- paste(title, ".pdf")
  ggsave(title_pdf)
}




#get recordSets with missing data 

getMissingSampleWithoutPreserverationMode <- function(con){
  rs <- dbSendQuery(con,"select locations.id as location_id, episodes.case_id as case_id, locations.\"name\" as location_name, patients.\"data\" ->> 'patientID' as patient_name
, patients.id, \"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' as preservation_mode from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    
                    where \"episodeForms\".name like 'form_35_ver-6' 
                    and \"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' is null;")
  
  
  missing_preservation_mode <- as_tibble(fetch(rs, n=-1))
  return(missing_preservation_mode)
}

getMissingSampleRecordSet <-function(con){
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,  
\"episodeForms\".data ->> 'urn:ccdg:dataelement:89:3' as \"Year of Sample\",
\"episodeForms\".data ->> 'urn:ccdg:dataelement:56:2' as \"SampleID\",
\"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' as preservation_mode,
\"episodeForms\".data ->> 'urn:ccdg:dataelement:54:2' as \"Material type\",


\"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
left join episodes on \"episodeForms\".episode_id = episodes.id 
left join cases on public.cases.id = episodes.case_id
left join patients on patients.id = cases.patient_id
left join locations on locations.id = cases.location_id

where 
(
\"episodeForms\".data ->> 'urn:ccdg:dataelement:89:3' is null
or
\"episodeForms\".data ->> 'urn:ccdg:dataelement:56:2' is null
or
\"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' is null
or
\"episodeForms\".data ->> 'urn:ccdg:dataelement:54:2' is null
)

and 

\"episodeForms\".name like '%form_35%' 

order by patients.id;")
  
  sample <- as_tibble(fetch(rs, n=-1))
  return(sample)
}

getMissingHistoRecordSet <-function(con){
  
  rs <- dbSendQuery(con,"select locations.id as locations_id, locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:68:2' as Localization_of_metastasis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:92:1' as Localization_of_primary_tumor,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:91:1' as Morphology,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:83:1' as Grade,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:77:1'  as Regional_lymph_nodes,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:71:1'  as Primary_Tumor,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:75:1'  as Distant_metastasis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:73:3'  as UICC_version,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:70:2'  as UICC_Stage,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:68:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:92:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:91:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:83:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:77:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:71:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:75:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:73:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:70:2' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_34%' 
                    
                    order by patients.id;")
  
  histo <- as_tibble(fetch(rs, n=-1))
  return(histo)
  
}

getMissingSurgeryRecordSet <-function(con){
  #Surgery
  
  rs <- dbSendQuery(con,"
select patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, locations.name as location_name,
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:8:3' as time_difference_between_initial_diagnosis_and_surger,
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:9:2' as surgery_radicality,
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:49:1' as surgery_type,
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:93:1' as location_of_the_tumor,
                  
                  \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                  left join episodes on \"episodeForms\".episode_id = episodes.id 
                  left join cases on public.cases.id = episodes.case_id
                  left join patients on patients.id = cases.patient_id
                  left join locations on locations.id = cases.location_id
                  
                  where 
                  (
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:8:3' is null
                  or
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:9:2' is null
                  or
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:49:1' is null
                  or
                  \"episodeForms\".data ->> 'urn:ccdg:dataelement:93:1' is null
                  )
                  
                  and 
                  
                  \"episodeForms\".name like '%form_32%' 
                  
                  order by patients.id;")
  
  surgery <- as_tibble(fetch(rs, n=-1))

  return(surgery)
}

getMissingPatientRecordSet <-function(con){
  #Patient
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:3:1' as Age_at_diagnosis_rounded_to_years,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:85:1' as Biological_sex,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:5:2' as Vital_status,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:7:2' as Overall_survival_status,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:88:1' as Colonoscopy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:31:3' as CT,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:61:5' as Liver_imaging,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:63:4' as Lung_imaging,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:30:3' as MRI,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:15:2' as Mismatch_repair_gene_expression,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:14:3' as Microsatellite_instability,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:20:3' as KRAS_exon_2_codons_12_or_13,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:21:5' as KRAS_exon_3_codons_59_or_61,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:25:3' as KRAS_exon_4_codons_117_or_146_mutation_status,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:3:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:85:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:5:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:7:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:88:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:31:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:61:5' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:63:4' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:30:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:15:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:14:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:20:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:21:5' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:25:3' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_28%' 
                    
                    order by patients.id;")
  
  patient <- as_tibble(fetch(rs, n=-1))
  return(patient)
}

getMissingTargetedTherapyRecordSet <-function(con){
  # targeted therapy
  
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,  
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:35:3' as date_of_start_of_targeted_therapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:36:1' as time_of_end_in_weeks_since_initial_diagnosis,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:35:3' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:36:1' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_30%' 
                    
                    order by patients.id;")
  
  targeted_therapy <- as_tibble(fetch(rs, n=-1))
  return(targeted_therapy)
}

getMissingPharmacotherapyRecordSet <-function(con){
  
  
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,  
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:10:2' as Date_of_start_of_pharamacotherapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:11:2' as Date_of_end_of_pharamcotherapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:59:5' as Scheme_of_pharmacotherapy,
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:10:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:11:2' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:59:5' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_33%' 
                    
                    order by patients.id
                    ;")
  
  pharmacotherapy <- as_tibble(fetch(rs, n=-1))
  return(pharmacotherapy)
}

getMissingRadiationTherapyRecordSet <-function(con){
  
  # Radiation Therapy
  
  
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:12:4' as Start_of_the_radiation_therapy_in_weeks_since_diagnosis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:13:2' as End_of_the_radiation_therapy_in_weeks_since_diagnosis,
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:12:4' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:13:2' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_29%' 
                    
                    order by patients.id
                    ;")
  
  radiationtherapy <- as_tibble(fetch(rs, n=-1))
  return(radiationtherapy)
}

getMissingResponseToTherapyRecordSet <-function(con){ 
  
  rs <- dbSendQuery(con,"select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:34:1' as date_response_was_obtained_in_weeks_since_initial_diagnosis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:33:1' as specific_response,
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    (
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:34:1' is null
                    or
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:33:1' is null
                    )
                    
                    and 
                    
                    \"episodeForms\".name like '%form_31%' 
                    
                    order by patients.id
                    ;")
  
  
  response_to_therapy <- as_tibble(fetch(rs, n=-1))
  return(response_to_therapy)
  }

#get recordSets

getAllSampleRecordSet <-function(con){
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,  
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:89:3' as \"Year of Sample\",
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:56:2' as \"SampleID\",
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' as preservation_mode,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:54:2' as \"Material type\",
                    
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    
                    \"episodeForms\".name like '%form_35%' 
                    
                    order by patients.id;")
  
  sample <- as_tibble(fetch(rs, n=-1))
  return(sample)
}

getAllHistoRecordSet <-function(con){
  
  rs <- dbSendQuery(con,"select locations.id as locations_id, locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:68:2' as Localization_of_metastasis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:92:1' as Localization_of_primary_tumor,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:91:1' as Morphology,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:83:1' as Grade,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:77:1'  as Regional_lymph_nodes,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:71:1'  as Primary_Tumor,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:75:1'  as Distant_metastasis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:73:3'  as UICC_version,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:70:2'  as UICC_Stage,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    
                    
                    where 
                    
                    \"episodeForms\".name like '%form_34%' 
                    
                    order by patients.id;")
  
  histo <- as_tibble(fetch(rs, n=-1))
  return(histo)
  
}

getAllSurgeryRecordSet <-function(con){
  #Surgery
  
  rs <- dbSendQuery(con,"
                    select patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, locations.name as location_name,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:8:3' as time_difference_between_initial_diagnosis_and_surger,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:9:2' as surgery_radicality,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:49:1' as surgery_type,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:93:1' as location_of_the_tumor,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    
                    \"episodeForms\".name like '%form_32%' 
                    
                    order by patients.id;")
  
  surgery <- as_tibble(fetch(rs, n=-1))
  
  return(surgery)
}

getAllPatientRecordSet <-function(con){
  #Patient
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:3:1' as Age_at_diagnosis_rounded_to_years,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:85:1' as Biological_sex,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:5:2' as Vital_status,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:7:2' as Overall_survival_status,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:88:1' as Colonoscopy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:31:3' as CT,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:61:5' as Liver_imaging,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:63:4' as Lung_imaging,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:30:3' as MRI,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:15:2' as Mismatch_repair_gene_expression,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:14:3' as Microsatellite_instability,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:20:3' as KRAS_exon_2_codons_12_or_13,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:21:5' as KRAS_exon_3_codons_59_or_61,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:25:3' as KRAS_exon_4_codons_117_or_146_mutation_status,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    
                    \"episodeForms\".name like '%form_28%' 
                    
                    order by patients.id;")
  
  patient <- as_tibble(fetch(rs, n=-1))
  return(patient)
}

getAllTargetedTherapyRecordSet <-function(con){
  # targeted therapy
  
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,  
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:35:3' as date_of_start_of_targeted_therapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:36:1' as time_of_end_in_weeks_since_initial_diagnosis,
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    
                    \"episodeForms\".name like '%form_30%' 
                    
                    order by patients.id;")
  
  targeted_therapy <- as_tibble(fetch(rs, n=-1))
  return(targeted_therapy)
}

getAllPharmacotherapyRecordSet <-function(con){
  
  
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,  
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:10:2' as Date_of_start_of_pharamacotherapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:11:2' as Date_of_end_of_pharamcotherapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:59:5' as Scheme_of_pharmacotherapy,
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    
                    \"episodeForms\".name like '%form_33%' 
                    
                    order by patients.id
                    ;")
  
  pharmacotherapy <- as_tibble(fetch(rs, n=-1))
  return(pharmacotherapy)
}

getAllRadiationTherapyRecordSet <-function(con){
  
  # Radiation Therapy
  
  
  rs <- dbSendQuery(con,"
                    select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:12:4' as Start_of_the_radiation_therapy_in_weeks_since_diagnosis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:13:2' as End_of_the_radiation_therapy_in_weeks_since_diagnosis,
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    
                    \"episodeForms\".name like '%form_29%' 
                    
                    order by patients.id
                    ;")
  
  radiationtherapy <- as_tibble(fetch(rs, n=-1))
  return(radiationtherapy)
}

getAllResponseToTherapyRecordSet <-function(con){ 
  
  rs <- dbSendQuery(con,"select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id, 
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:34:1' as date_response_was_obtained_in_weeks_since_initial_diagnosis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:33:1' as specific_response,
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    
                    \"episodeForms\".name like '%form_31%' 
                    
                    order by patients.id
                    ;")
  
  
  response_to_therapy <- as_tibble(fetch(rs, n=-1))
  return(response_to_therapy)
}

getSampleRecordSetWithPreserverationModeFFPE <- function(con){
  rs <- dbSendQuery(con,"select locations.id as location_id, episodes.case_id as case_id, locations.\"name\" as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id, \"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' as preservation_mode from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    
                    where \"episodeForms\".name like 'form_35_ver-6' 
                    and \"episodeForms\".data ->> 'urn:ccdg:dataelement:55:2' is 'null'FFPE';")
  
  
  missing_preservation_mode <- as_tibble(fetch(rs, n=-1))
  return(missing_preservation_mode)
}

getAllPatientsWithLocations <-function(con){
  rs <- dbSendQuery(con,"select 
patients.\"data\" ->> 'patientID' as patient_name,
                    patients.id,
                    patient_locations.location_id,
                    locations.\"name\"
                    from patients,
                    (select patient_id, location_id from patients_locations) as patient_locations,
                    (select id, name from locations) as locations
                    where patients.id = patient_locations.patient_id
                    and locations.id = patient_locations.location_id;")
  
  
  patients_with_locations <- as_tibble(fetch(rs, n=-1))
  
  names(patients_with_locations)[4]<-"location_name"
  
  return(patients_with_locations) 
}

getAllPatientsWithtTNMStageConspicuousness <- function(tnmPrimaryTumor, tnmRegionalLymphNodes, tnmDistantMetastasis, uiccVersion, uiccStage, patient_name, location_name){
  #print("getAllPatientsWithtTNMStageConspicuousness")
  #print(uiccVersion)
  #TODO implement and call validation tnm-methodes
  if(is.na(uiccVersion)){
    print(patient_name)
    print(location_name)
    print("has nA Value") 
  }
  else if(uiccVersion=="5th edition"){
    print("5th edition")
  } 
  else if(uiccVersion=="6th edition"){
    print("6th edition")
      
  }
  else if(uiccVersion=="7th edition"){
    print("7th edition")    
  }
  else if(uiccVersion=="8th edition"){
    print("8th edition")    
  }
  else{
    print(uiccVersion)
  }
    
  
  #tnmPrimaryTumor
  #tnmRegionalLymphNodes
  #tnmDistantMetastasis
  #uiccVersion
  #uiccStage
}

getAllTherapysandReponsesTogether <-function(con){
  rs <- dbSendQuery(con,"select locations.name as location_name, patients.\"data\" ->> 'patientID' as patient_name, patients.id as patient_id,

                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:4:3' as time_of_recurrence_metastasis_diagnosis,
                    
                    (\"episodeForms\".data ->> 'urn:ccdg:dataelement:34:1')::int as date_response_was_obtained_in_weeks_since_initial_diagnosis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:33:1' as specific_response,
                    
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:35:3' as date_of_start_of_targeted_therapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:36:1' as time_of_end_in_weeks_since_initial_diagnosis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:10:2' as Date_of_start_of_pharamacotherapy,
                    
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:11:2' as Date_of_end_of_pharamcotherapy,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:59:5' as Scheme_of_pharmacotherapy,
                    
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:12:4' as Start_of_the_radiation_therapy_in_weeks_since_diagnosis,
                    \"episodeForms\".data ->> 'urn:ccdg:dataelement:13:2' as End_of_the_radiation_therapy_in_weeks_since_diagnosis,
                    
                    
                    \"episodeForms\".name, locations.id as location_id, episodes.case_id as case_id from \"episodeForms\" 
                    left join episodes on \"episodeForms\".episode_id = episodes.id 
                    left join cases on public.cases.id = episodes.case_id
                    left join patients on patients.id = cases.patient_id
                    left join locations on locations.id = cases.location_id
                    
                    where 
                    \"episodeForms\".name like '%form_28%' --time of recurrence                 
                    or
                    \"episodeForms\".name like '%form_31%' --response
                    or 
                    \"episodeForms\".name like '%form_30%' --targeted therapy
                    or
                    \"episodeForms\".name like '%form_33%' --pharmacotherapy
                    or
                    \"episodeForms\".name like '%form_29%' --radiation therapy
                    
                    order by location_name, patient_id, date_response_was_obtained_in_weeks_since_initial_diagnosis, specific_response, time_of_recurrence_metastasis_diagnosis"
  )
  
  
  therapysAndReponsesTogether <- as_tibble(fetch(rs, n=-1))
  return(therapysAndReponsesTogether) 
}

getListsoFDataFramesWithCountsOfAllValues <- function(con){
  
  sample_counts <- count(getAllSampleRecordSet(con), "location_name")
  names(sample_counts) <- c("location_name", "sample_missings_values")
  
  histo_counts <- count(getAllHistoRecordSet(con), "location_name")
  names(histo_counts) <- c("location_name", "histo_missings_values")
  
  surgery_counts <- count(getAllSurgeryRecordSet(con), "location_name")
  names(surgery_counts) <- c("location_name", "surgery_missings_values") 
  
  patient_counts <- count(getAllPatientRecordSet(con), "location_name")
  names(patient_counts) <- c("location_name", "patient_missing_values") 
  
  targetedTherapy_counts <- count(getAllTargetedTherapyRecordSet(con), "location_name")
  names(targetedTherapy_counts) <- c("location_name", "targetedtherapy_missing_values") 
  
  pharmacotherapy_counts <- count(getAllPharmacotherapyRecordSet(con), "location_name")
  names(pharmacotherapy_counts) <- c("location_name", "pharmacotherapy_missing_values") 
  
  radiationtherapy_counts <- count(getAllRadiationTherapyRecordSet(con), "location_name")
  names(radiationtherapy_counts) <- c("location_name", "radiationtherapy_missing_values") 
  
  responseToTherapy_counts <- count(getAllResponseToTherapyRecordSet(con), "location_name")
  names(responseToTherapy_counts) <- c("location_name", "responsetotherapy_missing_values") 
  
  return(list(sample_counts, histo_counts, surgery_counts, patient_counts, targetedTherapy_counts, pharmacotherapy_counts, radiationtherapy_counts, responseToTherapy_counts))
  
  
  }

getListsoFDataFramesWithCountsOfAllMissingValues <- function(con){
  
  sample_missings_counts <- count(getMissingSampleRecordSet(con), "location_name")
  names(sample_missings_counts) <- c("location_name", "sample_missings_values")
  
  histo_missings_counts <- count(getMissingHistoRecordSet(con), "location_name")
  names(histo_missings_counts) <- c("location_name", "histo_missings_values")
  
  surgery_missings_counts <- count(getMissingSurgeryRecordSet(con), "location_name")
  names(surgery_missings_counts) <- c("location_name", "surgery_missings_values") 
  
  patient_missings_counts <- count(getMissingPatientRecordSet(con), "location_name")
  names(patient_missings_counts) <- c("location_name", "patient_missing_values") 
  
  targetedTherapy_missings_counts <- count(getMissingTargetedTherapyRecordSet(con), "location_name")
  names(targetedTherapy_missings_counts) <- c("location_name", "targetedtherapy_missing_values") 
  
  pharmacotherapy_missings_counts <- count(getMissingPharmacotherapyRecordSet(con), "location_name")
  names(pharmacotherapy_missings_counts) <- c("location_name", "pharmacotherapy_missing_values") 
  
  radiationtherapy_missings_counts <- count(getMissingRadiationTherapyRecordSet(con), "location_name")
  names(radiationtherapy_missings_counts) <- c("location_name", "radiationtherapy_missing_values") 
  
  responseToTherapy_missings_counts <- count(getMissingResponseToTherapyRecordSet(con), "location_name")
  names(responseToTherapy_missings_counts) <- c("location_name", "responsetotherapy_missing_values") 
  
  return(list(sample_missings_counts, histo_missings_counts, surgery_missings_counts, patient_missings_counts, targetedTherapy_missings_counts, pharmacotherapy_missings_counts, radiationtherapy_missings_counts, responseToTherapy_missings_counts))
}

getListsoFDataFramesWithCountsOfAllMissingValues <- function(con){
  
  sample_missings <- getMissingSampleRecordSet(con)
  names(sample_missings) <- c("location_name", "sample_missings_values")
  
  histo_missings <- getMissingHistoRecordSet(con)
  names(histo_missings) <- c("location_name", "histo_missings_values")
  
  surgery_missings <- getMissingSurgeryRecordSet(con)
  names(surgery_missings) <- c("location_name", "surgery_missings_values") 
  
  patient_missings <- getMissingPatientRecordSet(con)
  names(patient_missings) <- c("location_name", "patient_missing_values") 
  
  targetedTherapy_missings <- getMissingTargetedTherapyRecordSet(con)
  names(targetedTherapy_missings) <- c("location_name", "targetedtherapy_missing_values") 
  
  pharmacotherapy_missings <- getMissingPharmacotherapyRecordSet(con)
  names(pharmacotherapy_missings) <- c("location_name", "pharmacotherapy_missing_values") 
  
  radiationtherapy_missings <- getMissingRadiationTherapyRecordSet(con)
  names(radiationtherapy_missings) <- c("location_name", "radiationtherapy_missing_values") 
  
  responseToTherapy_missings <- getMissingResponseToTherapyRecordSet(con)
  names(responseToTherapy_missings) <- c("location_name", "responsetotherapy_missing_values") 
  
  return(list(sample_missings, histo_missings, surgery_missings, patient_missings, targetedTherapy_missings, pharmacotherapy_missings, radiationtherapy_missings, responseToTherapy_missings))
}




getCountFormsPerBiobank <- function(con){
  #identical to getListOfData...
  
  sample_counts <- count(getAllSampleRecordSet(con), "location_name")
  names(sample_counts) <- c("location_name", "sample_values")
  
  histo_counts <- count(getAllHistoRecordSet(con), "location_name")
  names(histo_counts) <- c("location_name", "histo_values")
  
  surgery_counts <- count(getAllSurgeryRecordSet(con), "location_name")
  names(surgery_counts) <- c("location_name", "surgery_values") 
  
  patient_counts <- count(getAllPatientRecordSet(con), "location_name")
  names(patient_counts) <- c("location_name", "patient_values") 
  
  targetedTherapy_counts <- count(getAllTargetedTherapyRecordSet(con), "location_name")
  names(targetedTherapy_counts) <- c("location_name", "targetedtherapy_values") 
  
  pharmacotherapy_counts <- count(getAllPharmacotherapyRecordSet(con), "location_name")
  names(pharmacotherapy_counts) <- c("location_name", "pharmacotherapy_values") 
  
  radiationtherapy_counts <- count(getAllRadiationTherapyRecordSet(con), "location_name")
  names(radiationtherapy_counts) <- c("location_name", "radiationtherapy_values") 
  
  responseToTherapy_counts <- count(getAllResponseToTherapyRecordSet(con), "location_name")
  names(responseToTherapy_counts) <- c("location_name", "responsetotherapy_values") 
  
  count_values_per_biobank <- Reduce(function(x,y) merge(x=x, y=y, by = "location_name", all = TRUE), list(sample_counts, histo_counts, surgery_counts, patient_counts, targetedTherapy_counts, pharmacotherapy_counts, radiationtherapy_counts, responseToTherapy_counts))
  
  count_values_per_biobank_transformed <- melt(count_values_per_biobank[,c('location_name', 
                                                                                           'sample_values', 'histo_values', 'surgery_values',
                                                                                           'patient_values','targetedtherapy_values',
                                                                                           'pharmacotherapy_values', 'radiationtherapy_values', 
                                                                                           'responsetotherapy_values')],id.vars = 1)
  return(count_values_per_biobank_transformed)
}


getCountFormsWithMissingValuesPerBiobank <- function(con){
  
  #identical to getListOfDataFrames... could be extracted to own function
  sample_missings_counts <- count(getMissingSampleRecordSet(con), "location_name")
  names(sample_missings_counts) <- c("location_name", "sample_missing_values")
  
  histo_missings_counts <- count(getMissingHistoRecordSet(con), "location_name")
  names(histo_missings_counts) <- c("location_name", "histo_missing_values")
  
  surgery_missings_counts <- count(getMissingSurgeryRecordSet(con), "location_name")
  names(surgery_missings_counts) <- c("location_name", "surgery_missings_values") 
  
  patient_missings_counts <- count(getMissingPatientRecordSet(con), "location_name")
  names(patient_missings_counts) <- c("location_name", "patient_missing_values") 
  
  targetedTherapy_missings_counts <- count(getMissingTargetedTherapyRecordSet(con), "location_name")
  names(targetedTherapy_missings_counts) <- c("location_name", "targetedtherapy_missing_values") 
  
  pharmacotherapy_missings_counts <- count(getMissingPharmacotherapyRecordSet(con), "location_name")
  names(pharmacotherapy_missings_counts) <- c("location_name", "pharmacotherapy_missing_values") 
  
  radiationtherapy_missings_counts <- count(getMissingRadiationTherapyRecordSet(con), "location_name")
  names(radiationtherapy_missings_counts) <- c("location_name", "radiationtherapy_missing_values") 
  
  responseToTherapy_missings_counts <- count(getMissingResponseToTherapyRecordSet(con), "location_name")
  names(responseToTherapy_missings_counts) <- c("location_name", "responsetotherapy_missing_values") 
  
  count_missing_values_per_biobank <- Reduce(function(x,y) merge(x=x, y=y, by = "location_name", all = TRUE), list(sample_missings_counts, histo_missings_counts, surgery_missings_counts, patient_missings_counts, targetedTherapy_missings_counts, pharmacotherapy_missings_counts, radiationtherapy_missings_counts, responseToTherapy_missings_counts))
  
  #count_missing_values_per_biobank_transformed <- melt(count_missing_values_per_biobank[,c('location_name', 'sample_missing_values', 'histo_missing_values','patient_missing_values','pharmacotherapy_missing_values', 'radiationtherapy_missing_values', 'surgery_missings_values' )],id.vars = 1)
  
  
  count_missing_values_per_biobank_transformed <- melt(count_missing_values_per_biobank[,c('location_name', 
                                      'sample_missing_values', 'histo_missing_values', 'surgery_missings_values',
                                      'patient_missing_values','targetedtherapy_missing_values',
                                      'pharmacotherapy_missing_values', 'radiationtherapy_missing_values', 'responsetotherapy_missing_values')],id.vars = 1)
  
  
  
  return(count_missing_values_per_biobank_transformed)
}

getPatientsWithPreservationModeBUTWithoutFFPE <- function(connection){
  
  samplesWithoutPreservationMode <- getMissingSampleWithoutPreserverationMode(connection)
  #TODO: Why is sum of with and without > allsampleRecords???
  #TODO: check if also patients without any samples are included in this test
  allsampleRecords <- getAllSampleRecordSet(connection)
  allsampleRecordsWithoutFFPE <- subset(allsampleRecords, preservation_mode!="FFPE")
  allsampleRecordsWithFFPE <- subset(allsampleRecords, preservation_mode=="FFPE")

  patientsWithoutFFPEValues <- setdiff(allsampleRecordsWithoutFFPE[,"patient_name"], allsampleRecordsWithFFPE[,"patient_name"])
  
  #patientsWithoutFFPEAndNotDetectedByRequieredTest <- subset(patientsWithoutFFPEValues, patient_name %in% samplesWithoutPreservationMode)
  
  patientsWithPreservationModeBUTWithoutFFPE <- subset(patientsWithoutFFPEValues, !(patient_name %in% samplesWithoutPreservationMode$patient_name))
  
  allPAtientsWithLocation = getAllPatientsWithLocations(connection)
  recordsetWithPreservationModeBUTWithoutFFPE <- subset(allPAtientsWithLocation, patient_name %in% patientsWithPreservationModeBUTWithoutFFPE$patient_name)
  
  #Note:rename clumn name to column_name, with index it's not super nice
  names(recordsetWithPreservationModeBUTWithoutFFPE)[4]<-"location_name"
  
  return(recordsetWithPreservationModeBUTWithoutFFPE)
}

getPatientsWithoutSurgery <- function(connection){
  
  allSamplesWithSurgerys = getAllSurgeryRecordSet(connection)
  allPatientsWithLocation = getAllPatientsWithLocations(connection)
  patientsWithoutLocations <- subset(allPatientsWithLocation, !(patient_name %in% allSamplesWithSurgerys$patient_name))
  
  #Note:rename clumn name to column_name, with index it's not super nice
  names(patientsWithoutLocations)[4]<-"location_name"
  
  return(patientsWithoutLocations)
  
}

getPatientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt <- function(connection){
  
  allTherapysAndResponsesTogether <- getAllTherapysandReponsesTogether(connection)
  
  uniquePatientswithTherapysandResponses <- unique(allTherapysAndResponsesTogether$patient_name)
  uniquePatientswithTherapysandResponses <- as.vector(uniquePatientswithTherapysandResponses)
  
  patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt <- list()
  
  
  for(specificPatient in uniquePatientswithTherapysandResponses){
    #print(specificPatient)
    tattGroupedByPatient <- subset(allTherapysAndResponsesTogether, patient_name==specificPatient)
    
    #(tattGroupedByPatient)
    
    #check if patient has complete_response and what the last therapy was
    patientconspicuousness <- NA
    
    lasttherapy <- "none"
    complete_response <- FALSE
    complete_response_but_its_not_the_last_therapy <- FALSE
    loopIterator <- 0
    numberOfLastCompleteEntry <- 0
    
    for(specific_response in tattGroupedByPatient$specific_response){
      loopIterator = loopIterator + 1
      #paste("specific_response", specific_response, sep=" ")
      #print(data.frame(specific_response))
      if(!is.na(specific_response)){
        
        if(str_detect(specific_response,"Complete")){
          complete_response <- TRUE
          numberOfLastCompleteEntry <- loopIterator
          #print(complete_response)
        }
      }
      if(!is.na(specific_response)){
        lasttherapy <- specific_response
      }
      
    }
    #patient has a complete response buzt last therapy is not complete_response
    if(complete_response==TRUE&!str_detect(lasttherapy,"Complete")){
      print("complete_response==TRUE")
      print(specificPatient)
      print(lasttherapy)
      complete_response_but_its_not_the_last_therapy <- TRUE
      
    }

#TESTEST

    if(str_detect(lasttherapy,"Complete")){
	print("start testtest")
	print(specificPatient)
	print(lasttherapy)

     #tattGroupedByPatient$date_response_was_obtained_in_weeks_since_initial_diagnosis[is.infinite(tattGroupedByPatient$date_response_was_obtained_in_weeks_since_initial_diagnosis)] <- 0
     #tattGroupedByPatient$date_of_end_of_pharamcotherapy[is.infinite(tattGroupedByPatient$date_of_end_of_pharamcotherapy)] <- 0
     #tattGroupedByPatient$end_of_the_radiation_therapy_in_weeks_since_diagnosis[is.infinite(tattGroupedByPatient$end_of_the_radiation_therapy_in_weeks_since_diagnosis)] <- 0



      latestResponseDate <- max(as.numeric(tattGroupedByPatient$date_response_was_obtained_in_weeks_since_initial_diagnosis), na.rm=TRUE)
      dateEndOfPharmaTherapy <- max(as.numeric(tattGroupedByPatient$date_of_end_of_pharamcotherapy), na.rm=TRUE)
      dateEndOfRadiationTherapy <- max(as.numeric(tattGroupedByPatient$end_of_the_radiation_therapy_in_weeks_since_diagnosis), na.rm=TRUE)


      latestResponseDate[is.na(latestResponseDate)] <- 0
       dateEndOfPharmaTherapy[is.na(dateEndOfPharmaTherapy)] <- 0
       dateEndOfRadiationTherapy[is.na(dateEndOfRadiationTherapy)] <- 0

       latestResponseDate[is.infinite(latestResponseDate)] <- 0
       dateEndOfPharmaTherapy[is.infinite(dateEndOfPharmaTherapy)] <- 0
       dateEndOfRadiationTherapy[is.infinite(dateEndOfRadiationTherapy)] <- 0


#	print(latestResponseDate)
#	print(dateEndOfPharmaTherapy)
#	print(dateEndOfRadiationTherapy)
	print("testest2")
        if(as.numeric(latestResponseDate)<as.numeric(dateEndOfPharmaTherapy) || as.numeric(latestResponseDate)<as.numeric(dateEndOfRadiationTherapy)){
        print(paste("PatientConspicousness", specificPatient, sep = " "))
	print(specificPatient)
	
#         patientconspicuousness <- "noProgressivDiseasOrTimeOfRecurrenceAfterCompleteReponse"
	patient_name <- c(tattGroupedByPatient$patient_name)
	
	patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt[[patient_name[1]]] <- patient_name[1]
}

print(as.numeric(tattGroupedByPatient$date_response_was_obtained_in_weeks_since_initial_diagnosis))
print(as.numeric(tattGroupedByPatient$date_of_start_of_pharamacotherapy))
print(as.numeric(tattGroupedByPatient$start_of_the_radiation_therapy_in_weeks_since_diagnosis))
print(as.numeric(tattGroupedByPatient$tattGroupedByPatient$date_of_start_of_targeted_therapy))

      latestResponseDate <- max(as.numeric(tattGroupedByPatient$date_response_was_obtained_in_weeks_since_initial_diagnosis), na.rm=TRUE)
      dateStartOfPharmaTherapy <- max(as.numeric(tattGroupedByPatient$date_of_start_of_pharamacotherapy), na.rm=TRUE)
      dateStartOfRadiationTherapy <- max(as.numeric(tattGroupedByPatient$start_of_the_radiation_therapy_in_weeks_since_diagnosis), na.rm=TRUE)
	  dateStartOfTargetedTherapy <- max(as.numeric(tattGroupedByPatient$tattGroupedByPatient$date_of_start_of_targeted_therapy), na.rm=TRUE)


       latestResponseDate[is.na(latestResponseDate)] <- 0
       dateStartOfPharmaTherapy[is.na(dateStartOfPharmaTherapy)] <- 0
       dateStartOfRadiationTherapy[is.na(dateStartOfRadiationTherapy)] <- 0
       dateStartOfTargetedTherapy[is.na(dateStartOfTargetedTherapy)] <- 0
	   
	   
       latestResponseDate[is.infinite(latestResponseDate)] <- 0
       dateStartOfPharmaTherapy[is.infinite(dateStartOfPharmaTherapy)] <- 0
       dateStartOfRadiationTherapy[is.infinite(dateStartOfRadiationTherapy)] <- 0
       dateStartOfTargetedTherapy[is.infinite(dateStartOfTargetedTherapy)] <- 0	   


        print(latestResponseDate)
        print(dateStartOfPharmaTherapy)
        print(dateStartOfRadiationTherapy)
		print(dateStartOfTargetedTherapy)
		
        print("testest2")
        if(as.numeric(latestResponseDate)<as.numeric(dateStartOfPharmaTherapy) || as.numeric(latestResponseDate)<as.numeric(dateStartOfRadiationTherapy) || as.numeric(latestResponseDate)<as.numeric(dateStartOfTargetedTherapy)){
        print(paste("PatientConspicousness", specificPatient, sep = " "))
        print(specificPatient)

#         patientconspicuousness <- "noProgressivDiseasOrTimeOfRecurrenceAfterCompleteReponse"
        patient_name <- c(tattGroupedByPatient$patient_name)

        patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt[[patient_name[1]]] <- patient_name[1]


 
      }
}
#TESTTESTEND


    
    #find if progressiv disease or time of recurrence after complete response exists 
    if(complete_response_but_its_not_the_last_therapy){
      #print(tattGroupedByPatient)
      #print(numberOfLastCompleteEntry)
      
      patientconspicuousness <- "noProgressivDiseasOrTimeOfRecurrenceAfterCompleteReponse"
      
      loopIterator <-0
      patient_specific_response_value <- c(tattGroupedByPatient$specific_response)
      patient_time_of_recurrence_metastasis_diagnosis <- c(tattGroupedByPatient$time_of_recurrence_metastasis_diagnosis)
      patient_name <- c(tattGroupedByPatient$patient_name)
      #patient_location <- c(tattGroupedByPatient$patient_location)
      patient.df <- data.frame(patient_specific_response_value, patient_time_of_recurrence_metastasis_diagnosis, patient_name)
      
      latestResponseDate <- max(as.numeric(tattGroupedByPatient$date_response_was_obtained_in_weeks_since_initial_diagnosis), na.rm=TRUE)
      dateEndOfPharmaTherapy <- max(as.numeric(tattGroupedByPatient$date_of_end_of_pharamcotherapy), na.rm=TRUE)
      dateEndOfRadiationTherapy <- max(as.numeric(tattGroupedByPatient$end_of_the_radiation_therapy_in_weeks_since_diagnosis), na.rm=TRUE)


      latestResponseDate[is.na(latestResponseDate)] <- 0
      dateEndOfPharmaTherapy[is.na(dateEndOfPharmaTherapy)] <- 0
      dateEndOfRadiationTherapy[is.na(dateEndOfRadiationTherapy)] <- 0
      
      if(latestResponseDate>=dateEndOfPharmaTherapy && latestResponseDate>=dateEndOfRadiationTherapy){
        #Latest Response is not older then last Therapy
#TODO comment in to deactivate false positve cases       
# patientconspicuousness <- NA
      }

      for(patientValues in 1:nrow(patient.df)){
        loopIterator = loopIterator +1
        if(loopIterator>numberOfLastCompleteEntry){
          #print("loop over multiple values")
          #print(patientValues)
          
          #if(str_detect(patient_name, "WRPNVC2NOAX")){
          #  patient_name
          #}
          
          if(!is.na(patient.df[loopIterator, "patient_specific_response_value"])){
            if(str_detect(patient.df[loopIterator, "patient_specific_response_value"], "Progressive")){
              #Progressive after Complete Response, data set to not conspicous
              #print(paste(patient_name, " Progressive after Complete Response, data set to not conspicous"))
              patientconspicuousness <- NA
            }
          }
          
          if(!is.na(patient.df[loopIterator, "patient_time_of_recurrence_metastasis_diagnosis"])){
            #Recurrence metastasis diagnosis after Complete Response, data set to not conspicous
            #print(paste(patient_name, " Recurrence metastasis diagnosis after Complete Response, data set to not conspicous"))
            patientconspicuousness <- NA
          }        
          #print(data.frame(data.df[loopIterator,2],data.df[,3]))
        }
      }
    }
    
    if(!is.na(patientconspicuousness)){
      print(patientconspicuousness)
      print(patient_name[1])
      print(numberOfLastCompleteEntry)
      #list.append(patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt, patient_name=patientconspicuousness)
      #patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt[[patient_name[1]]] <- patientconspicuousness
      #TODO: this not the way how the return values should stored
      patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt[[patient_name[1]]] <- patient_name[1]
      #patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt[["patient_name"]] <- patient_name[1]
      
    }
    
    #print(complete_response)
    #paste("complete_response", complete_response, sep=" ")
    #print(lasttherapy)
  }
  #print("loop finished")
  
  return(patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt)
}



writeFilterdValueInCSVFile <- function(listOfData, filter){
  
  
  filter_txt <- paste(filter, ".txt", sep="")
  write.table(listOfData, filter_txt )

}

writeListValuesIntoXLSXFile <- function(listOfData, filename, sheetName, fileNamePrefix = "", appendToExistingSheet = FALSE){
  
  #filename <- paste(filename, fileNamePrefix, sep= "")
  
  filename_xlsx <- paste(filename, "xlsx", sep=".")
  

  if(!appendToExistingSheet){
    write.xlsx2(listOfData, file = filename_xlsx, sheetName = sheetName, append = TRUE)   
  }

  if(appendToExistingSheet){
    existing.data <- NA
    #existing.data <- tryCatch(readExcelSheet(filename_xlsx, sheetName))
    #existing.data <- read.xlsx2(file = filename_xlsx, sheetName = sheetName )
    #new.data <- rbind(existing.data, listOfData)
    #write.xlsx2(listOfData, file = filename_xlsx, sheetName = sheetName, append = FALSE)   
    
    tryCatch({
      print("tryCatch add rows to worksheet")
      
      print(filename)
      print(sheetName)
      existing.data <- read.xlsx2(file = filename_xlsx, sheetName = sheetName )
      existing.data[1] <- NULL
      #TODO: existing.data has crappy extra columns
      listOfData <- rbind.fill(existing.data, listOfData)
      print(listOfData)
      
      wb = loadWorkbook(filename_xlsx)
      removeSheet(wb, sheetName = sheetName)
      saveWorkbook(wb, filename_xlsx)
      print("after saveWorkbook")
      
      write.xlsx2(listOfData, file = filename_xlsx, sheetName = sheetName, append = TRUE) 
      print("after saveWorkbook write xlsx")
    },
    error = function (condition) {
      print(condition)
      print("No Sheet existing")
      print(filename)
      print(sheetName)
      #new.data <- rbind(existing.data, listOfData)
      write.xlsx2(listOfData, file = filename_xlsx, sheetName = sheetName, append = TRUE) 
    }
    )
  }

}

#readExcelSheet <- function(filename, sheetName){
  
#  existing.data <- read.xlsx2(file = filename, sheetName = sheetName )
#  return(existing.data)
#}



createXLSXreportPerBiobank <- function(connection){
  createSheetsWithMissingValues()
  
  #First additional test petr asked for
  # Finds all patients which have a Preservation Mode Sample, but non with FFPE, Patients with a sample but without any preservation mode should already be catched by getMissingSampleRecordSet
  patientsWithPreservationModeBUTWithoutFFPE <- getPatientsWithPreservationModeBUTWithoutFFPE(connection)
  patientsWithPreservationModeBUTWithoutFFPE["warning"] <- "Patient which has a preservation mode sample, but non with FFPE "
  
  for(location in unique(patientsWithPreservationModeBUTWithoutFFPE$location_name)){
    #writeListValuesIntoXLSXFile(patientsWithPreservationModeBUTWithoutFFPE[patientsWithPreservationModeBUTWithoutFFPE$location_name==location[1],], location[1], "extrareport_warnings1_PreservationModeButWithoutFFPE")
    writeListValuesIntoXLSXFile(patientsWithPreservationModeBUTWithoutFFPE[patientsWithPreservationModeBUTWithoutFFPE$location_name==location[1],], location[1], "extrareport_warnings1", appendToExistingSheet = TRUE)
  }
  
  #Second additional test
  #Patients without surgery
  patientsWithoutSurgery <- getPatientsWithoutSurgery(connection)
  patientsWithoutSurgery["NOTE"] <- "Patient without surgery"
  
  
  for(location in unique(patientsWithoutSurgery$location_name)){
    #writeListValuesIntoXLSXFile(patientsWithoutSurgery[patientsWithoutSurgery$location_name==location[1],], location[1], "extrareport_warnings1_PatientWithoutSurgery")
    writeListValuesIntoXLSXFile(patientsWithoutSurgery[patientsWithoutSurgery$location_name==location[1],], location[1], "extrareport_warnings1", appendToExistingSheet = TRUE)
  }
  
  writeIntoExclePatientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt(connection)
  

}

writeIntoExclePatientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt <-function(connection){
  patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt <- getPatientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt(connection)
  allPatientsWithLocation <-getAllPatientsWithLocations(connection)
  
  combined_list <- list()
  for(patient in patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt){
    
    subset_list <- subset(allPatientsWithLocation, patient_name==patient)
    combined_list <- bind_rows(combined_list, subset_list)
  }
  combined_list["NOTE"] <- "Patient with new treatment after CompleteResponse, but no ProgressiveDisease or TimeofRecurrence after it - Note could include false-positive cases"
  
  
  for(location in unique(combined_list$location_name)){
    #writeListValuesIntoXLSXFile(combined_list[combined_list$location_name==location[1],], location[1], "extrareport_warnings1_patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt")
    writeListValuesIntoXLSXFile(combined_list[combined_list$location_name==location[1],], location[1], "extrareport_warnings1", appendToExistingSheet = TRUE)
  }
  
}

createSheetsWithMissingValues <-function(){
  #Missing Values
  missingSampleRecordSet <- getMissingSampleRecordSet(connection)
  
  for(location in unique(missingSampleRecordSet$location_name)){
    writeListValuesIntoXLSXFile(missingSampleRecordSet[missingSampleRecordSet$location_name==location[1],], location[1], "Sample_Values")
  }
  
  missingHistoRecordSet <- getMissingHistoRecordSet(connection)
  
  for(location in unique(missingHistoRecordSet$location_name)){
    writeListValuesIntoXLSXFile(missingHistoRecordSet[missingHistoRecordSet$location_name==location[1],], location[1], "Histopathology_Values")
  }
  
  surgeryRecordSet <- getMissingSurgeryRecordSet(connection)
  
  for(location in unique(surgeryRecordSet$location_name)){
    writeListValuesIntoXLSXFile(surgeryRecordSet[surgeryRecordSet$location_name==location[1],], location[1], "Surgery_Values")
  }
  
  patientRecordSet <- getMissingPatientRecordSet(connection)
  
  for(location in unique(patientRecordSet$location_name)){ 
    writeListValuesIntoXLSXFile(patientRecordSet[patientRecordSet$location_name==location[1],], location[1], "Patient_Values")
  }
  
  targethedTherapyRecordSet <- getMissingTargetedTherapyRecordSet(connection)
  
  for(location in unique(targethedTherapyRecordSet$location_name)){ 
    writeListValuesIntoXLSXFile(targethedTherapyRecordSet[targethedTherapyRecordSet$location_name==location[1],], location[1], "TargethedTherapy_Values") 
  }
  
  radiationtherapyRecordSet <- getMissingRadiationTherapyRecordSet(connection)
  
  for(location in unique(radiationtherapyRecordSet$location_name)){
    writeListValuesIntoXLSXFile(radiationtherapyRecordSet[radiationtherapyRecordSet$location_name==location[1],], location[1], "radiationtherapy_Values")
  }
  
  responseToTherapyRecordSet <- getMissingResponseToTherapyRecordSet(connection)
  
  for(location in unique(responseToTherapyRecordSet$location_name)){
    writeListValuesIntoXLSXFile(responseToTherapyRecordSet[responseToTherapyRecordSet$location_name==location[1],], location[1], "responseToTherapy_Values")
  }
  
}

moveFilesFromWorkingDirectoryToOtherFolder <- function(filePattern, newMainDir, newSubDir){
  
  mainDir <- newMainDir
  
  #subDir <- paste0(format(Sys.time(), "%Y%m%d_%H%M_"), "report")
  subDir <- newSubDir
  print(paste0("move files from"))
  dir.create(file.path(mainDir, subDir))
  
  print(paste0("move files from", getwd()))
  print(paste0("move files to", paste0(mainDir,subDir)))
  
  curlist.of.files <- list.files(getwd(), filePattern)
  file.copy(curlist.of.files, paste0(mainDir,subDir))
  
  file.remove(curlist.of.files)
  
  return(paste0(mainDir,subDir))
  
}

addExtraReporttoReport <- function(extrareportDirectory){
  tryCatch({
  extrareport.files <- list.files(extrareportDirectory, "reported-")
  extrareport.dataset <- list()
  for(file in extrareport.files){
    #TODO: if we have more then one report-file, we need to change this part
    extrareport.dataset <- read.xlsx2(file = extrareport.files, sheetName = "Sheet1" )
    extrareport.dataset[1] <- NULL
  }
  
  for(location in unique(extrareport.dataset$locationName)){
    
    writeListValuesIntoXLSXFile(extrareport.dataset[extrareport.dataset$locationName==location[1],], location[1], "extrareport_warnings2", appendToExistingSheet = TRUE)
    #print(extrareport.dataset[extrareport.dataset$locationName==location[1],])
  }
  },
  error = function (condition) {
    print("could not execute addExtrReporttoReport")
    print(condition)
  
  })
}

zipFolderWithPassword <- function(pathToZip, filename, dirWhereZipShouldBeSaved, password){
  
  #PASSWORD = "crc_qualit_checks_$2018"
  #dirToZip <- "C:/Users/stampe/Documents/20181011_0853_report"
  files2zip <-dir(pathToZip, full.names = TRUE)
  print(dirWhereZipShouldBeSaved)
  print(paste(pathToZip, filename, sep = "/"))
  
  zip(zipfile = paste(dirWhereZipShouldBeSaved, filename, sep = "/"), files = files2zip, flags = paste("-r9XjP ", password))
}


# main start
createPlosts = FALSE
createOverviewPlotsOfMissingValuesPerLocation = FALSE
createXLSXreportOfMissingValuesPerBiobank = TRUE
accessFidleAroundCode = FALSE

connection <- getDBConnection()


if (createPlosts){
  createPlotWithSampleYears(connection)
  createPlotWithoutSampleYears(connection)
  createPlotWithoutSampleID(connection)
  createPlotWithoutPreservationMode(connection)
  createPlotWithoutMaterialType(connection)
  createPlotsWithoutHistoValues(connection)
  createPlotsWithoutSurgeryValues(connection)
  createPlotsWithoutPatientValues(connection)
  createPlotsWithoutTargetedTherapy(connection)
  createPlotsWithoutPharmacotherapy(connection)
  createPlotsWithoutRadiationTherapy(connection)
  createPlotsWithoutResponseToTherapy(connection)
}


if (createOverviewPlotsOfMissingValuesPerLocation){
  # count missing values per BioBank overview
  countMissingValuesPerBioBank =getCountFormsWithMissingValuesPerBiobank(connection)
  
  #Greate Plot for every location
  for(location in unique(countMissingValuesPerBioBank$location_name)){
    createPlotForAllMissedValues(connection, countMissingValuesPerBioBank, location[1], location[1],paste(location[1], " Counting Forms with missing entrys", sep=' '))
  }
  
  pathtofolderToZip <- moveFilesFromWorkingDirectoryToOtherFolder(".pdf",  paste0(getwd(), "/"), paste0(format(Sys.time(), "%Y%m%d_%H%M_"), "report"))
  
  
  
}


if(createXLSXreportOfMissingValuesPerBiobank){

  createXLSXreportPerBiobank(connection)
  addExtraReporttoReport(getwd())
  
  pathtofolderToZip <- moveFilesFromWorkingDirectoryToOtherFolder(".xlsx", paste0(getwd(), "/"), paste0(format(Sys.time(), "%Y%m%d_%H%M_"), "report"))
  
  
  zipFolderWithPassword(pathToZip = pathtofolderToZip, filename = "qualityReports.zip", dirWhereZipShouldBeSaved = "/var/lib/tomcat7/webapps/report/", password = "myzippassword")
  
}


if(accessFidleAroundCode){

  
  zipFolderWithPassword <- function(pathToZip, filename, dirWhereZipShouldBeSaved, password){
    
    #PASSWORD = "crc_qualit_checks_$2018"
    #dirToZip <- "C:/Users/stampe/Documents/20181011_0853_report"
    files2zip <-dir(pathToZip, full.names = TRUE)
    print(dirWhereZipShouldBeSaved)
    print(paste(pathToZip, filename, sep = "/"))
    zip(zipfile = 'qualitychecks.zip', files = files2zip, flags = paste("-r9XjP ", password))
    
    
  }
  
  PASSWORD = "crc_qualit_checks_$2018"
  dirToZip <- "C:/Users/stampe/Documents/20181011_0853_report"

  files2zip <-dir(dirToZip, full.names = TRUE)
  
  zip(zipfile = 'qualitychecks.zip', files = files2zip, flags = paste("-r9XjP ", PASSWORD))
  
  
  
  PASSWORD <- "BillMurray"
  
  zip("C:/Users/user/Downloads/myarchive.zip", 
      files="C:/Users/user/Downloads/example.txt", 
      flags = paste("--password", PASSWORD))
  
  
  
  
mainDir <- "."
subDir <- paste0(format(Sys.time(), "%Y%m%d_%H%M_"), "report")
dir.create(file.path(mainDir, subDir))

curlist.of.files <- list.files(getwd(), "reported-")
file.copy(curlist.of.files, subDir)
  
file.remove(curlist.of.files)

oldDate <- format(Sys.Date()-8, "%Y%m%d")

dirs <- list.dirs()
dirs <- dirs[ grepl("_report", dirs) ]
dirs <- dirs[ grepl(oldDate, dirs) ]
unlink(dirs, recursive = TRUE)
getwd()



addExtraReporttoReport <- function(extrareportDirectory){
  extrareport.files <- list.files(extrareportDirectory, "reported-")
  extrareport.dataset <- list()
  for(file in extrareport.files){
    #TODO: if we have more then one report-file, we need to change this part
    extrareport.dataset <- read.xlsx2(file = extrareport.files, sheetName = "Sheet1" )
    extrareport.dataset[1] <- NULL
  }
  
  for(location in unique(extrareport.dataset$locationName)){
    
    writeListValuesIntoXLSXFile(extrareport.dataset[extrareport.dataset$locationName==location[1],], location[1], "extrareport_warnings2", appendToExistingSheet = TRUE)
    #print(extrareport.dataset[extrareport.dataset$locationName==location[1],])
  }
}






existing.data <- read.xlsx2(file = extrareport.files, sheetName = sheetName )

#First additional test petr asked for
# Finds all patients wich have a Preservation Mode Sample, but non with FFPE, Patients with a sample but without any preservation mode should already be catched by getMissingSampleRecordSet
patientsWithPreservationModeBUTWithoutFFPE <- getPatientsWithPreservationModeBUTWithoutFFPE(connection)

#Second additional test
#Patients without surgery
patientsWithoutSurgery <- getPatientsWithoutSurgery(connection)


#Third test, tnm vs uicc validation - tnm validation functions not implement yet
allHistRecords <- getAllHistoRecordSet(connection)
#mapply(getAllPatientsWithtTNMStageConspicuousness, allHistRecords$primary_tumor,allHistRecords$regional_lymph_nodes, allHistRecords$distant_metastasis, allHistRecords$uicc_version, allHistRecords$uicc_stage, allHistRecords$patient_name, allHistRecords$location_name)



#Fourth test, therapy and reponse logic
#TODO check if order in every case is correct

patientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt <- getPatientsWhereNewTreatmentAfterCompleteResponseButNoProgressiveDiseaseOrTimeofRecurrenceAfterIt(connection)



##### other tests

colums_with_missing_entrys <- Reduce(function(x,y) merge(x=x, y=y, by = "patient_id", all.x = TRUE), list(getMissingSampleRecordSet(connection), getMissingHistoRecordSet(connection), getMissingSurgeryRecordSet(connection), getMissingPatientRecordSet(connection), getMissingTargetedTherapyRecordSet(connection), getMissingPharmacotherapyRecordSet(connection), getMissingRadiationTherapyRecordSet(connection), getMissingResponseToTherapyRecordSet(connection)))

listWithCountsOfAllMissingValues <- getListsoFDataFramesWithCountsOfAllMissingValues(connection)


listsOfDataFramesWithCountsOfAllValues = getListsoFDataFramesWithCountsOfAllValues(connection)

# count values per BioBank overview
countValuesPerBioBank = getCountFormsPerBiobank(connection)
# count missing values per BioBank overview
countMissingValuesPerBioBank =getCountFormsWithMissingValuesPerBiobank(connection)

#draw all missing values in one plot
ggplot(countMissingValuesPerBioBank,aes(x = location_name,y = value)) + ggtitle("Count of missed value to fullfill the required field requirement") +  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_log10() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#tattGroupedByPatients <- subset(allTherapysAndResponsesTogether, patient_name=="graz1")
#getMissingPharmacotherapyRecordSet(connection)
#count(getMissingPatientRecordSet(connection), "location_name")
#allResponseToTherapyRecordSet <- getAllResponseToTherapyRecordSet(connection)
#allTargetedTherapyRecordSet <- getAllTargetedTherapyRecordSet(connection)
#allHistRecords$primary_tumor
#for(i in allHistRecords){print(i)}


#testsss <- distinct(countWithMissingValuesPerBioBank,location_name)
#for(i in testsss){
#  testsss <- testsss + i
#}

#ggplot(countWithMissingValuesPerBioBank[countWithMissingValuesPerBioBank$location_name=="Biobank Graz",],aes(x = location_name,y = value)) + ggtitle("Count of missed value to fullfill the required field requirement") +  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_log10() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


}



dbDisconnect(connection)


