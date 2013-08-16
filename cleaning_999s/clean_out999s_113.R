#113: 999 cleaning
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source('scripts/cleaning_999s/999_functions.R')

##############
########health
##############
h_113 <- read.csv("raw_data/113/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv",
                  stringsAsFactors=F)
h_113 <- subset(h_113, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_113$uuid <- sapply(paste(h_113$geocodeoffacility, h_113$photo), FUN=digest)
h_113 <- subset(h_113, !duplicated(h_113$uuid))
      # OUTPUT SHOULD BE 0
      anyDuplicated(h_113$uuid)
h <- h_113

#changing into real NAs
for (i in 1:422)
{
  h[,i] <- recodeVar(h[,i], "n/a", NA)  
}

#CLEANING
h$km_to_referral_facility <- as.numeric(h$km_to_referral_facility)
cellst(h, 'km_to_referral_facility',
       which(h$km_to_referral_facility > 500), NA_integer_)
      
h$generator_months_broken <- as.numeric(h$generator_months_broken)
cellst(h, 'generator_months_broken',
       which(h$generator_months_broken > 360), NA_integer_)

h$grid_months_broken <- as.numeric(h$grid_months_broken)
cellst(h, 'grid_months_broken',
       which(h$grid_months_broken > 180), NA_integer_)

h$slab_pit_latrine_number <- as.numeric(h$slab_pit_latrine_number)
cellst(h, 'slab_pit_latrine_number',
       which(h$slab_pit_latrine_number > 31), NA_integer_)

h$inpatient_care_num_beds <- as.numeric(h$inpatient_care_num_beds)
cellst(h, 'inpatient_care_num_beds',
       which(h$inpatient_care_num_beds > 300), NA_integer_)

h$registration_price <- as.numeric(h$registration_price)
cellst(h, 'registration_price',
       which(h$registration_price > 5000), NA_integer_)

h$contraceptives_price <- as.numeric(h$contraceptives_price)
cellst(h, 'contraceptives_price',
       which(h$contraceptives_price > 8500), NA_integer_)

h$anc_delivery_price <- as.numeric(h$anc_delivery_price)
cellst(h, 'anc_delivery_price',
       which(h$anc_delivery_price > 900), NA_integer_)

h$immunization_price <- as.numeric(h$immunization_price)
cellst(h, 'immunization_price',
       which(h$immunization_price > 5000), NA_integer_)

h$tb_treatment_price <- as.numeric(h$tb_treatment_price)
cellst(h, 'immunization_price',
       which(h$immunization_price == 9999), NA_integer_)

h$malaria_treatment_price <- as.numeric(h$malaria_treatment_price)
cellst(h, 'malaria_treatment_price',
       which(h$malaria_treatment_price == 9999), NA_integer_)

h$lab_testing_price <- as.numeric(h$lab_testing_price)
cellst(h, 'lab_testing_price',
       which(h$lab_testing_price > 5000), NA_integer_)

h$inpatient_stay_price <- as.numeric(h$inpatient_stay_price)
cellst(h, 'inpatient_stay_price',
       which(h$inpatient_stay_price > 5000), NA_integer_)

h$medication_price <- as.numeric(h$medication_price)
cellst(h, 'inpatient_stay_price',
       which(h$inpatient_stay_price > 5000), NA_integer_)

#WRITING OUT
write.csv(h, "in_process_data/999cleaned/Health_113_999Cleaned.csv", row.names=F)


##############
#####education
##############
e_113 <- read.csv("raw_data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a"))
e_113 <- subset(e_113, subset=(gps != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
e_113$uuid <- sapply(paste(e_113$gps, e_113$photo), FUN=digest)

# OUTPUT SHOULD BE 0
anyDuplicated(e_113$uuid)
e <- e_113

#changing into real NAs
for (i in 1:800)
{
  e[,i] <- recodeVar(e[,i], "n/a", NA)  
}

#changing "none" to numeric 0
for (i in 1:800)
{
  e[,i] <- recodeVar(e[,i], "none", 0)  
}

#changing "o" to numeric 0
for (i in 1:800)
{
  e[,i] <- recodeVar(e[,i], "o", 0)  
}


#CLEANING            

e$km_to_catchment_area <- as.numeric(e$km_to_catchment_area)        
e$km_to_catchment_area[e$km_to_catchment_area > 800] <- NA_integer_ 

e$km_to_secondary_school <- as.numeric(e$km_to_secondary_school)    
e$km_to_secondary_school[e$km_to_secondary_school > 800] <- NA_integer_

e$num_students_frthr_than_3km <- as.numeric(e$num_students_frthr_than_3km)
cellst(e, 'num_students_frthr_than_3km',which(e$num_students_frthr_than_3km == 999), NA_integer_) # 999 considered as outlier
cellst(e, 'num_students_frthr_than_3km',which(e$num_students_frthr_than_3km > 9990), NA_integer_) 

e$days_no_electricity <- as.numeric(e$days_no_electricity)

e$days_no_water_pastmth <- as.numeric(e$days_no_water_pastmth)

e$days_no_potable_water_pastmth <- as.numeric(e$days_no_potable_water_pastmth)

e$flush_toilet_number<- as.numeric(e$flush_toilet_number)

e$flush_toilet_not_working <- as.numeric(e$flush_toilet_not_working)

e$vip_latrine_number <- as.numeric(e$vip_latrine_number)

e$vip_latrine_not_working <- as.numeric(e$vip_latrine_not_working)

e$slab_pit_latrine_number <- as.numeric(e$slab_pit_latrine_number)

e$slab_pit_latrine_not_working <- as.numeric(e$slab_pit_latrine_not_working)

e$open_pit_latrine_number <- as.numeric(e$open_pit_latrine_number)

e$open_pit_latrine_not_working <- as.numeric(e$open_pit_latrine_not_working)

e$bucket_system_number <- as.numeric(e$bucket_system_number)

e$bucket_system_not_working <- as.numeric(e$bucket_system_not_working)

e$other_toilets_number <- as.numeric(e$other_toilets_number)

e$other_toilets_not_working <- as.numeric(e$other_toilets_not_working)

e$times_building_cleaned_lastmth <- as.numeric(e$times_building_cleaned_lastmth)

e$times_trash_disposed_lastmth <- as.numeric(e$times_trash_disposed_lastmth)

e$num_tchrs_total <- as.numeric(e$num_tchrs_total)
cellst(e, 'num_tchrs_total',which(e$num_tchrs_total < 0), NA_integer_) #entry of negative number eliminate

e$num_tchrs_male_full_time <- as.numeric(e$num_tchrs_male_full_time)

e$num_tchrs_male_part_time <- as.numeric(e$num_tchrs_male_part_time)

e$num_tchrs_female_full_time <- as.numeric(e$num_tchrs_female_full_time)

e$num_tchrs_female_part_time <- as.numeric(e$num_tchrs_female_part_time)

e$tchrs_male_below_ssce <- as.numeric(e$tchrs_male_below_ssce)
cellst(e, 'tchrs_male_below_ssce',which(e$tchrs_male_below_ssce >900), NA_integer_) 

e$tchrs_female_below_ssce <- as.numeric(e$tchrs_female_below_ssce)
cellst(e, 'tchrs_female_below_ssce',which(e$tchrs_female_below_ssce >900), NA_integer_) 

e$tchrs_male_ssce_wasc <- as.numeric(e$tchrs_male_ssce_wasc)
cellst(e, 'tchrs_male_ssce_wasc',which(e$tchrs_male_ssce_wasc == 99), NA_integer_) #one data entry was 99  

e$tchrs_female_ssce_wasc <- as.numeric(e$tchrs_female_ssce_wasc)
cellst(e, 'tchrs_female_ssce_wasc',which(e$tchrs_female_ssce_wasc == 999), NA_integer_) #one data entry was 999

e$tchrs_male_grade2 <- as.numeric(e$tchrs_male_grade2)
cellst(e, 'tchrs_male_grade2',which(e$tchrs_male_grade2 == 9999), NA_integer_) #one data entry was 9999

e$tchrs_female_grade2 <- as.numeric(e$tchrs_female_grade2)
cellst(e, 'tchrs_female_grade2',which(e$tchrs_female_grade2 == 9999), NA_integer_) #one data entry was 9999

e$tchrs_male_ond <- as.numeric(e$tchrs_male_ond)
cellst(e, 'tchrs_male_ond',which(e$tchrs_male_ond == 999), NA_integer_) #one data entry was 999

e$tchrs_female_ond <- as.numeric(e$tchrs_female_ond)
cellst(e, 'tchrs_female_ond',which(e$tchrs_female_ond == 999), NA_integer_) #one data entry was 999

e$tchrs_male_nce <- as.numeric(e$tchrs_male_nce)
cellst(e, 'tchrs_male_nce',which(e$tchrs_male_nce == 999), NA_integer_)
cellst(e, 'tchrs_male_nce',which(e$tchrs_male_nce < 0), NA_integer_)

e$tchrs_female_nce <- as.numeric(e$tchrs_female_nce)
cellst(e, 'tchrs_female_nce',which(e$tchrs_female_nce == 9999), NA_integer_)
cellst(e, 'tchrs_female_nce',which(e$tchrs_female_nce < 0), NA_integer_)

e$tchrs_male_pgde <- as.numeric(e$tchrs_male_pgde)
cellst(e, 'tchrs_male_pgde',which(e$tchrs_male_pgde == 999), NA_integer_)

e$tchrs_female_pgde <- as.numeric(e$tchrs_female_pgde)
cellst(e, 'tchrs_female_pgde', which(e$tchrs_female_pgde == 9999), NA_integer_)

e$tchrs_male_b_ed <- as.numeric(e$tchrs_male_b_ed)
cellst(e, 'tchrs_male_b_ed', which(e$tchrs_male_b_ed == 998), NA_integer_)

e$tchrs_female_b_ed <- as.numeric(e$tchrs_female_b_ed)
cellst(e, 'tchrs_female_b_ed', which(e$tchrs_female_b_ed == 999), NA_integer_)

e$tchrs_male_other_w_nce <- as.numeric(e$tchrs_male_other_w_nce)
cellst(e, 'tchrs_male_other_w_nce', which(e$tchrs_male_other_w_nce == 99), NA_integer_) #99 considered as outlier

e$tchrs_female_other_w_nce <- as.numeric(e$tchrs_female_other_w_nce)
cellst(e, 'tchrs_female_other_w_nce', which(e$tchrs_female_other_w_nce == 999), NA_integer_)

e$tchrs_female_other_wo_nce <- as.numeric(e$tchrs_female_other_wo_nce)
cellst(e, 'tchrs_female_other_wo_nce', which(e$tchrs_female_other_wo_nce == 999), NA_integer_)

e$num_sr_staff_male <-as.numeric(e$num_sr_staff_male)

e$num_sr_staff_female <-as.numeric(e$num_sr_staff_female)

e$num_sr_staff_total <-as.numeric(e$num_sr_staff_total)
cellst(e, 'num_sr_staff_total', which(e$num_sr_staff_total < 0), NA_integer_)

e$num_jr_staff_male <-as.numeric(e$num_jr_staff_male)

e$num_jr_staff_female <-as.numeric(e$num_jr_staff_female)

e$num_jr_staff_total <-as.numeric(e$num_jr_staff_total)
cellst(e, 'num_jr_staff_total', which(e$num_jr_staff_total < 0), NA_integer_)

e$days_school_understaffed <- as.numeric(e$days_school_understaffed)

e$days_school_understaffed_closed <- as.numeric(e$days_school_understaffed_closed)

e$school_max_num_students <- as.numeric(e$school_max_num_students) #"I don't know" has eliminated
cellst(e, 'school_max_num_students', which(e$school_max_num_students == 9999), NA_integer_) #9999 has eliminated (there is 10800)

e$admit_more_num_students <- as.numeric(e$admit_more_num_students)
cellst(e, 'admit_more_num_students', which(e$admit_more_num_students == 9999), NA_integer_)

e$num_students_male <- as.numeric(e$num_students_male)

e$num_students_female <- as.numeric(e$num_students_female)

e$num_students_total <- as.numeric(e$num_students_total)

e$new_stdnts_enroll_fee <- as.numeric(e$new_stdnts_enroll_fee)


e$cont_stdnts_enroll_fee <- as.numeric(e$cont_stdnts_enroll_fee)
cellst(e, 'cont_stdnts_enroll_fee', which(e$cont_stdnts_enroll_fee < 0), NA_integer_)

e$textbooks_fee <- as.numeric(e$textbooks_fee)

e$materials_fee <- as.numeric(e$materials_fee)

e$uniforms_fee <- as.numeric(e$uniforms_fee)
cellst(e, 'uniforms_fee', which(e$uniforms_fee > 900000), NA_integer_) #one extreme value

e$transport_fee <- as.numeric(e$transport_fee)

e$exams_fee <- as.numeric(e$exams_fee)

e$pta_fee <- as.numeric(e$pta_fee)

e$sports_fee <- as.numeric(e$sports_fee)

e$sports_fee_exempt_yn[e$sports_fee_exempt_yn != "yes" & e$sports_fee_exempt_yn != "no"] <- NA #unless yes or no, eliminate

e$num_students_exempt <- as.numeric(e$num_students_exempt)  # answer of "all" and "o" eliminated
cellst(e, 'num_students_exempt', which(e$num_students_exempt > 8000), NA_integer_) #20 of "9999" has eliminated

e$in_kind_fees_yn[e$in_kind_fees_yn != "yes" & e$in_kind_fees_yn != "no"] <- NA

e$booklist_per_class_yn[e$booklist_per_class_yn != "yes" & e$booklist_per_class_yn != "no"] <- NA

e$annual_budget_amt_received <- as.numeric(e$annual_budget_amt_received)

e$num_tchrs_paid_fed_gov <- as.numeric(e$num_tchrs_paid_fed_gov)

e$num_tchrs_payrl_st_gov <- as.numeric(e$num_tchrs_payrl_st_gov)
cellst(e, 'num_tchrs_payrl_st_gov', which(e$num_tchrs_payrl_st_gov > 8000), NA_integer_)

e$num_tchrs_othr_payrl_st_gov <- as.numeric(e$num_tchrs_othr_payrl_st_gov)

e$num_tchrs_paid_loc_gov <- as.numeric(e$num_tchrs_paid_loc_gov)

e$num_tchrs_paid_prvt_for_profit <- as.numeric(e$num_tchrs_paid_prvt_for_profit)

e$num_tchrs_paid_prvt_non_profit <- as.numeric(e$num_tchrs_paid_prvt_non_profit)

e$num_tchrs_paid_other_src <- as.numeric(e$num_tchrs_paid_other_src)
cellst(e, 'num_tchrs_paid_other_src', which(e$num_tchrs_paid_other_src > 10000), NA_integer_) #one extreme value

e$num_tchrs_no_salary <- as.numeric(e$num_tchrs_no_salary)

e$num_students_scholarship <- as.numeric(e$num_students_scholarship)
cellst(e, 'num_students_scholarship', which(e$num_students_scholarship > 9000), NA_integer_)

e$scholarship_amt <- as.numeric(e$scholarship_amt)

e$times_tchr_pay_delay_pastyr <- as.numeric(e$times_tchr_pay_delay_pastyr)

e$times_tchr_pay_miss_pastyr <- as.numeric(e$times_tchr_pay_miss_pastyr)

e$num_library_materials <- as.numeric(e$num_library_materials) # there were 999s, but reasonable data

e$times_new_materials_added <- as.numeric(e$times_new_materials_added)
cellst(e, 'times_new_materials_added', which(e$times_new_materials_added == 999), NA_integer_)

e$num_classrms_good_cond <- as.numeric(e$num_classrms_good_cond)
cellst(e, 'num_classrms_good_cond', which(e$num_classrms_good_cond == 999), NA_integer_)

e$num_classrms_need_min_repairs <- as.numeric(e$num_classrms_need_min_repairs)
cellst(e, 'num_classrms_need_min_repairs', which(e$num_classrms_need_min_repairs == 999), NA_integer_)

e$num_classrms_need_maj_repairs <- as.numeric(e$num_classrms_need_maj_repairs)
cellst(e, 'num_classrms_need_maj_repairs', which(e$num_classrms_need_maj_repairs == 999), NA_integer_)

e$num_classrms_unused <- as.numeric(e$num_classrms_unused)
cellst(e, 'num_classrms_unused', which(e$num_classrms_unused >= 999), NA_integer_)

e$num_classrooms_multiple_use <- as.numeric(e$num_classrooms_multiple_use)
cellst(e, 'num_classrooms_multiple_use', which(e$num_classrooms_multiple_use >= 999), NA_integer_)

e$num_other_rooms <- as.numeric(e$num_other_rooms)
cellst(e,'num_other_rooms', which(e$num_other_rooms >= 999), NA_integer_)

e$num_sections_pry1 <- as.numeric(e$num_sections_pry1)
cellst(e,'num_sections_pry1', which(e$num_sections_pry1 >= 999), NA_integer_)

e$num_sections_pry2 <- as.numeric(e$num_sections_pry2)
cellst(e,'num_sections_pry2', which(e$num_sections_pry2 >= 999), NA_integer_)

e$num_sections_pry3 <- as.numeric(e$num_sections_pry3)
cellst(e,'num_sections_pry3', which(e$num_sections_pry3 >= 999), NA_integer_)

e$num_sections_pry4 <- as.numeric(e$num_sections_pry4)
cellst(e,'num_sections_pry4', which(e$num_sections_pry4 >= 999), NA_integer_)

e$num_sections_pry5 <- as.numeric(e$num_sections_pry5)
cellst(e,'num_sections_pry5', which(e$num_sections_pry5 >= 999), NA_integer_)

e$num_sections_pry6 <- as.numeric(e$num_sections_pry6)
cellst(e,'num_sections_pry6', which(e$num_sections_pry6 >= 999), NA_integer_)

e$num_sections_js1 <- as.numeric(e$num_sections_js1)
cellst(e,'num_sections_js1', which(e$num_sections_js1 >= 999), NA_integer_) # 8999 considered as typo of 9999

e$num_sections_js2 <- as.numeric(e$num_sections_js2)
cellst(e,'num_sections_js2', which(e$num_sections_js2 >= 899), NA_integer_) # 899 considered as typo of 999

e$num_sections_js3 <- as.numeric(e$num_sections_js3)
cellst(e,'num_sections_js3', which(e$num_sections_js3 >= 899), NA_integer_) # 899 considered as typo of 999

e$num_attached_benches <- as.numeric(e$num_attached_benches)
cellst(e,'num_attached_benches', which(e$num_attached_benches < 0), NA_integer_)
cellst(e,'num_attached_benches', which(e$num_attached_benches == 9999), NA_integer_) #considered 999 as reasonable number

e$num_attached_benches_unused <- as.numeric(e$num_attached_benches_unused)
cellst(e,'num_attached_benches_unused', which(e$num_attached_benches_unused >= 999), NA_integer_) #considered 99 as reasonable number

e$num_unattached_benches <- as.numeric(e$num_unattached_benches)

e$num_unattached_benches_unused <- as.numeric(e$num_unattached_benches_unused)

e$num_unattached_desks <- as.numeric(e$num_unattached_desks)

e$num_unattached_desks_unused <- as.numeric(e$num_unattached_desks_unused)

e$num_textbooks_english <- as.numeric(e$num_textbooks_english)

e$num_textbooks_math <- as.numeric(e$num_textbooks_math)

e$num_textbooks_social_sci <- as.numeric(e$num_textbooks_social_sci)

e$num_exercise_books <- as.numeric(e$num_exercise_books) # 2 of "dont know"


#extra

e$tchrs_male_other_wo_nce <- as.numeric(e$tchrs_male_other_wo_nce)
cellst(e,'tchrs_male_other_wo_nce', which(e$tchrs_male_other_wo_nce == 999), NA_integer_)

e$num_tchrs_attended_training <- as.numeric(e$num_tchrs_attended_training)
cellst(e,'num_tchrs_attended_training', which(e$num_tchrs_attended_training < 0), NA_integer_)

e$num_tchrs_attended_last_day <- as.numeric(e$num_tchrs_attended_last_day)
cellst(e,'num_tchrs_attended_last_day', which(e$num_tchrs_attended_last_day < 0), NA_integer_)

e$num_textbooks_pry_sci <- as.numeric(e$num_textbooks_pry_sci)


#clearing columns (names start from 'pry~' or 'js~')

check  <- 0 #checking process
check2 <- 0
check3 <- 0
check4 <- 0

temp3 <- e$js1_m_age_under12_2010_11
sort(temp3, TRUE)[1]

selcol = grep("^pry|^js",colnames(e)) #grab column names
for (i in 1:length(selcol))
{
  temp  <- e[,selcol[i]]
  temp  <- as.numeric(temp)
  temp[temp < 0] <- NA
  
  temp2<- temp
  hv <- sort(temp2,TRUE)[1]
  if((hv == 999999)|| (hv == 99999)||(hv == 9999)||(hv == 999))
  {
    temp[temp == hv] <- NA
    check <- check + 1
  }
  else if(hv%/%sort(temp2,TRUE)[2] > 100)  #eliminate extreme value outliers
  {
    temp[temp == hv] <- NA
    check2 <- check2 + 1
  }
  else 
  {  
    check3 <- check3 + 1
  }
  
  temp2 <- temp
  hv <- sort(temp2,TRUE)[1]
  if((hv == 99999)||(hv == 9999)||(hv == 999))
  {
    temp[temp == hv] <- NA
    check4 <- check4 + 1
  }
  else 
  {  
  }
  
  e[,selcol[i]] <- temp
}


#WRITING OUT
write.csv(e, "in_process_data/999cleaned/Education_113_999Cleaned.csv", row.names=F)

##############
#####Water
##############
# water -- no numerical questions (except for elevation...)
w_113 <- read.csv("raw_data/113/Water_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a"))
w_113$uuid <- sapply(paste(w_113$geo_id, w_113$photo), FUN=digest)
# OUTPUT SHOULD BE 0
anyDuplicated(w_113$uuid)

file.copy("raw_data/113/Water_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
            "in_process_data/999cleaned/Water_113_999Cleaned.csv", overwrite=T)


