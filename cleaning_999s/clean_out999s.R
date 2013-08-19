#setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("base_scripts/InstallFormhub.R")
source('cleaning_999s/999_functions.R')

extraschema = setNames(data.frame(rbind(
  c("mylga", "select one", "LGA"),
  c("mylga_state", "select one", "State")), stringsAsFactors=FALSE),
                       c("name", "type", "label"))
nastrings = c('999', '9999', '99999', '999999' , 'n/a')

merged_education <- data.frame(formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Education_661_Merged.csv",
                                 "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Education_05_06_2012.json",
                                extraForm = extraschema, na.strings=nastrings), stringsAsFactors=FALSE)
merged_health <- data.frame(formhubRead("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Health_661_Merged.csv",
                               "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/json_schemas/Health_17_04_2012.json",
                             extraForm = extraschema, na.strings=nastrings), stringsAsFactors=FALSE)

###################
#####Education#####
###################
##knocking out 999 values
cellst(merged_education, 
    c('num_students_total_gender.num_students_male','num_students_total_gender.num_students_female',
      'num_students_total_gender.num_students_total'),
    which(merged_education$num_students_total_gender.num_students_male %in% c(9991, 99919)), NA_integer_) # 10300 stays

cellst(merged_education, 'num_pry_total_gender.num_pry_female',
     which(merged_education$num_pry_total_gender.num_pry_female > 20000), NA_integer_)
cellst(merged_education, 'num_pry_total_gender.num_pry_male',
     which(merged_education$num_pry_total_gender.num_pry_male > 9990), NA_integer_)
cellst(merged_education, 'num_js_total_gender.num_js_male',
      which(merged_education$num_js_total_gender.num_js_male > 9989), NA_integer_)     
cellst(merged_education, 'num_toilet.num_toilet_boy',
  which(merged_education$num_toilet.num_toilet_boy > 9990), NA_integer_)
cellst(merged_education, 'num_toilet.num_toilet_girl',
      which(merged_education$num_toilet.num_toilet_girl > 9990), NA_integer_)
cellst(merged_education, 'num_toilet.num_toilet_both',
      which(merged_education$num_toilet.num_toilet_both > 9990), NA_integer_)
cellst(merged_education, 'num_tchrs.num_tchrs_male',
      which(merged_education$num_tchrs.num_tchrs_male > 9990), NA_integer_)
cellst(merged_education, 'num_tchrs.num_tchrs_female',
      which(merged_education$num_tchrs.num_tchrs_female > 9990), NA_integer_)
cellst(merged_education, 'manuals_js.num_math_textbook_js',
      which(merged_education$manuals_js.num_math_textbook_js > 5021000), NA_integer_)
cellst(merged_education, 'num_sr_staff_total',
      which(merged_education$num_sr_staff_total > 9990), NA_integer_)
cellst(merged_education, 'num_classrms_need_min_repairs',
      which(merged_education$num_classrms_need_min_repairs > 2998), NA_integer_)
cellst(merged_education, 'num_classrms_need_maj_repairs',
     which(merged_education$num_classrms_need_maj_repairs > 9990), NA_integer_)
cellst(merged_education, 'days_no_potable_water',
      which(merged_education$days_no_potable_water < 0 ), NA_integer_)
#cellst(merged_education, 'ratio_students_to_benches',
 #     which(merged_education$ratio_students_to_benches == "Inf"), NA_integer_)
#cellst(merged_education, 'ratio_students_to_desks',
 #     which(merged_education$ratio_students_to_desks == "Inf"), NA_integer_)
cellst(merged_education, 'num_ss_total_gender.num_ss_female',
      which(merged_education$num_ss_total_gender.num_ss_female > 6000 ), NA_integer_)
cellst(merged_education, c('num_ss_total_gender.num_ss_female', 'num_ss_total_gender.num_ss_male', 
                          'num_ss_total_gender.num_ss_total'),
      which(merged_education$num_ss_total_gender.num_ss_total > 19000 ), NA_integer_)
cellst(merged_education, 'km_to_secondary_school',
      which(merged_education$km_to_secondary_school > 9900 ), NA_integer_)
cellst(merged_education, 'num_toilet.num_toilet_both',
      which(merged_education$num_toilet.num_toilet_both > 50 ), NA_integer_)
cellst(merged_education, 'num_tchrs.num_tchrs_total',
      which(merged_education$num_tchrs.num_tchrs_total > 5000 ), NA_integer_)
cellst(merged_education, 'num_sr_staff_total',
      which(merged_education$num_sr_staff_total > 900 ), NA_integer_)
cellst(merged_education, 'num_classrms_good_cond',
      which(merged_education$num_classrms_good_cond > 9000 ), NA_integer_)
cellst(merged_education, 'num_classrms_need_maj_repairs',
      which(merged_education$num_classrms_need_maj_repairs > 515 ), NA_integer_)

#individual cells
cellst(merged_education, 'num_tchrs_qualification.num_tchrs_w_nce',
      which(merged_education$uuid =='6c7dd1bf-82c7-46e1-8ab0-ad85baada470' | merged_education$uuid =='41dff099-4e14-4fdf-9f49-300b7c1f5c8f' ), NA_integer_)
cellst(merged_education, c('num_tchrs.num_tchrs_total', 'num_tchrs.num_tchrs_female', 'num_students_total_gender.num_students_total', 'num_students_total_gender.num_students_male', 'num_tchrs.num_tchrs_male', 'num_students_total_gender.num_students_female'),
      which(merged_education$uuid =='ab52fe42-1525-46dd-945a-f5fef4566c16'), NA_integer_)
cellst(merged_education, 'num_classrms_total',
      which(merged_education$uuid =='fcdbb827-943b-40b5-bc59-19106759bf2c' | merged_education$uuid =='d17bde2e-a6ce-4b0d-87fe-ab39e61bfe8e'), NA_integer_)
cellst(merged_education, 'num_students_total_gender.num_students_total',
      which(merged_education$uuid =='cad7e54b-b5cc-4768-9c56-18a8f3f1023f'), NA_integer_)
cellst(merged_education, 'num_students_total_gender.num_students_total',
      which(merged_education$num_students_total_gender.num_students_total =='9699'), NA_integer_)
##total
write.csv(merged_education, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Education_661_999Cleaned.csv", row.names=FALSE)


###################
######Health######
###################
##knocking out 999 values
merged_health$not_for_private_1.toilets_available.num_bucket_system <- 
  as.numeric(merged_health$not_for_private_1.toilets_available.num_bucket_system)
merged_health$not_for_private_1.toilets_available.num_flush_or_pour_flush_piped <-
  as.numeric(merged_health$not_for_private_1.toilets_available.num_flush_or_pour_flush_piped)
cellst(merged_health, 'not_for_private_1.toilets_available.num_flush_or_pour_flush_piped',
      which(merged_health$not_for_private_1.toilets_available.num_flush_or_pour_flush_piped > 23000), NA_integer_)
cellst(merged_health, 'not_for_private_1.toilets_available.num_bucket_system',
      which(merged_health$not_for_private_1.toilets_available.num_bucket_system > 999), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_doctors_posted',
      which(merged_health$medical_staff_posted.num_doctors_posted > 999), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_midwives_posted',
      which(merged_health$medical_staff_posted.num_midwives_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.medical_records_officers_active',
      which(merged_health$medical_staff_active.medical_records_officers_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.lab_technicians_active',
      which(merged_health$medical_staff_active.lab_technicians_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.pharma_technicians_active',
      which(merged_health$medical_staff_active.pharma_technicians_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.pharmacists_active',
      which(merged_health$medical_staff_active.pharmacists_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.lab_technicians_active',
      which(merged_health$medical_staff_active.lab_technicians_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.pharmacists_active',
      which(merged_health$medical_staff_active.pharmacists_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.num_junior_chews_active',
      which(merged_health$medical_staff_active.num_junior_chews_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.num_nursemidwives_active',
      which(merged_health$medical_staff_active.num_nursemidwives_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.num_nurses_active',
      which(merged_health$medical_staff_active.num_nurses_active > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.medical_records_officers_posted',
      which(merged_health$medical_staff_posted.medical_records_officers_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.pharma_technicians_posted',
      which(merged_health$medical_staff_posted.pharma_technicians_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.lab_technicians_posted',
      which(merged_health$medical_staff_posted.lab_technicians_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.environmental_health_officers_posted',
      which(merged_health$medical_staff_posted.environmental_health_officers_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.pharmacists_posted',
      which(merged_health$medical_staff_posted.pharmacists_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_junior_chews_posted',
      which(merged_health$medical_staff_posted.num_junior_chews_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_chews_posted',
      which(merged_health$medical_staff_posted.num_chews_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_cho_posted',
      which(merged_health$medical_staff_posted.num_cho_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_nurses_posted',
      which(merged_health$medical_staff_posted.num_nurses_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_nursemidwives_posted',
      which(merged_health$medical_staff_posted.num_nursemidwives_posted > 980), NA_integer_)
cellst(merged_health, 'medical_staff_active.pharma_technicians_active',
      which(merged_health$medical_staff_active.pharma_technicians_active > 70), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_doctors_posted',
      which(merged_health$medical_staff_posted.num_doctors_posted > 580), NA_integer_)
cellst(merged_health, 'medical_staff_posted.num_nursemidwives_posted',
      which(merged_health$medical_staff_posted.num_nursemidwives_posted > 901), NA_integer_)
cellst(merged_health, 'medical_staff_posted.environmental_health_officers_posted',
      which(merged_health$medical_staff_posted.environmental_health_officers_posted > 901), NA_integer_)
cellst(merged_health, 'medical_staff_posted.pharma_technicians_posted',
      which(merged_health$medical_staff_posted.pharma_technicians_posted > 901), NA_integer_)
cellst(merged_health, 'medical_staff_posted.medical_records_officers_posted',
      which(merged_health$medical_staff_posted.medical_records_officers_posted > 66), NA_integer_)
cellst(merged_health, 'medical_staff_active.pharmacists_active',
      which(merged_health$medical_staff_active.pharmacists_active > 908), NA_integer_)
cellst(merged_health, 'medical_staff_active.medical_records_officers_active',
      which(merged_health$medical_staff_active.medical_records_officers_active > 908), NA_integer_)

write.csv(merged_health, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Health_661_999Cleaned.csv", row.names=FALSE)

# water -- no numerical questions
file.copy("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Water_661_Merged.csv", 
          "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_661_999Cleaned.csv")

# localities -- numerical questions already discarded in MergeDatasets.R
file.copy("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Local_661_Merged.csv",
          "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Localities_661_999Cleaned.csv")
