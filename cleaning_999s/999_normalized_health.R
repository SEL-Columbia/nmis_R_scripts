#####################################################################################################################
##Normalized Health 999 Cleaning ####################################################################################
#####################################################################################################################
source("base_scripts/InstallFormhub.R")
source('cleaning_999s/999_functions.R')

merged_health <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/Normalized/Health774.rds")

# numeric_column_list <-


num_names <- names(merged_health)[grep("^num.+", names(merged_health), )]



# numeric type conversion and ASSERTION(sort of)
merged_health <- numeric_batch(merged_health, numeric_column_list)

check_type <- batch_type(merged_health, numeric_column_list)
stopifnot(all(check_type %in% c("integer", "numeric")))


###################
######Health######
###################
##knocking out 999 values
# merged_health$num_bucket_system <- 
#   as.numeric(merged_health$num_bucket_system)
# merged_health$num_flush_or_pour_flush_piped <-
#   as.numeric(merged_health$num_flush_or_pour_flush_piped)

cellst(merged_health, 'num_flush_or_pour_flush_piped',
      which(merged_health$num_flush_or_pour_flush_piped > 23000), NA_integer_)

cellst(merged_health, 'num_bucket_system',
      which(merged_health$num_bucket_system > 999), NA_integer_)

cellst(merged_health, 'num_doctors_posted',
      which(merged_health$num_doctors_posted > 999), NA_integer_)

cellst(merged_health, 'num_midwives_posted',
      which(merged_health$num_midwives_posted > 980), NA_integer_)

cellst(merged_health, 'lab_technicians_active',
      which(merged_health$lab_technicians_active > 980), NA_integer_)

cellst(merged_health, 'lab_technicians_active',
      which(merged_health$lab_technicians_active > 980), NA_integer_)

cellst(merged_health, 'num_junior_chews_active',
      which(merged_health$num_junior_chews_active > 980), NA_integer_)

cellst(merged_health, 'num_nursemidwives_active',
      which(merged_health$num_nursemidwives_active > 980), NA_integer_)

cellst(merged_health, 'num_nurses_active',
      which(merged_health$num_nurses_active > 980), NA_integer_)

cellst(merged_health, 'lab_technicians_posted',
      which(merged_health$lab_technicians_posted > 980), NA_integer_)

cellst(merged_health, 'pharmacists_posted',
      which(merged_health$pharmacists_posted > 980), NA_integer_)

cellst(merged_health, 'num_junior_chews_posted',
      which(merged_health$num_junior_chews_posted > 980), NA_integer_)

cellst(merged_health, 'num_chews_posted',
      which(merged_health$num_chews_posted > 980), NA_integer_)

cellst(merged_health, 'num_cho_posted',
      which(merged_health$num_cho_posted > 980), NA_integer_)

cellst(merged_health, 'num_nurses_posted',
      which(merged_health$num_nurses_posted > 980), NA_integer_)

cellst(merged_health, 'num_nursemidwives_posted',
      which(merged_health$num_nursemidwives_posted > 980), NA_integer_)

cellst(merged_health, 'pharma_technicians_active',
      which(merged_health$pharma_technicians_active > 70), NA_integer_)

cellst(merged_health, 'num_doctors_posted',
      which(merged_health$num_doctors_posted > 580), NA_integer_)

cellst(merged_health, 'num_nursemidwives_posted',
      which(merged_health$num_nursemidwives_posted > 901), NA_integer_)

cellst(merged_health, 'environmental_health_officers_posted',
      which(merged_health$environmental_health_officers_posted > 901), NA_integer_)

cellst(merged_health, 'pharma_technicians_posted',
      which(merged_health$pharma_technicians_posted > 901), NA_integer_)

cellst(merged_health, 'medical_records_officers_posted',
      which(merged_health$medical_records_officers_posted > 66), NA_integer_)

cellst(merged_health, 'pharmacists_active',
      which(merged_health$pharmacists_active > 908), NA_integer_)

cellst(merged_health, 'medical_records_officers_active',
      which(merged_health$medical_records_officers_active > 908), NA_integer_)


#output
saveRDS(merged_health, "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Health_774_999Cleaned.rds")


