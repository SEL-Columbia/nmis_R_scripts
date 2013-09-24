# source("source_scripts/NMIS_Utils.R")
# source("base_scripts/InstallFormhub.R")


edu_661 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Education_661_Merged.csv", stringsAsFactors=F)
edu_113 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv",
                  stringsAsFactors=F, na.strings = c("NA", "n/a"))
edu_pilot <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/113/Pilot_Education_cleaned_2011Nov17.csv",
                    stringsAsFactors=F, na.strings = c("NA", "n/a"))

####
names(edu_661)[(which(!names(edu_661) %in% names(edu_113)))]

slugsearch <- function(nm, df=edu_661){
    names(df)[grep(nm, names(df), ignore.case=T)]
}
see <- function(nm, df=edu_113)
{
    table(df[,nm])
}

length(which(!names(edu_113) %in% names(edu_661)))
edu_113$num_students_male
####


rename(edu_113, c("days_no_potable_water_pastmth" = "days_no_potable_water",
                  "new_stdnts_enroll_fee" = "fees.admission_new", #### including fees.tuition_new??
                  "cont_stdnts_enroll_fee" = "fees.tuition_cont", 
                  "textbooks_fee" = "fees.textbook", 
                  "transport_fee" = "fees.transport", 
                  "exams_fee" = "fees.exam_fee", 
                  "pta_fee" = "fees.pta_fee", 
                  "num_unattached_desks" = "num_desks",
                  "num_textbooks_pry_sci" = "num_science_textbook_pry",
                  ""))

newname_113 <- c("days_no_electricity", "days_no_water_pastmth", "flush_toilet_number",
                 "flush_toilet_not_working", "vip_latrine_number", "vip_latrine_not_working",
                 "slab_pit_latrine_number", "slab_pit_latrine_not_working", "open_pit_latrine_number",
                 "open_pit_latrine_not_working", "bucket_system_not_working", "bucket_system_number",
                 "other_toilets_number", "other_toilets_not_working", "times_building_cleaned_lastmth",
                 "times_trash_disposed_lastmth", "num_tchrs_male_full_time", "num_tchrs_female_full_time",
                 "num_tchrs_female_part_time", "tchrs_male_below_ssce", "tchrs_female_below_ssce", 
                 "tchrs_male_ssce_wasc", "tchrs_female_ssce_wasc", "tchrs_male_grade2", 
                 "tchrs_female_grade2", "tchrs_male_ond", "tchrs_female_ond",
                 "tchrs_male_nce", "tchrs_female_nce", "tchrs_male_other_w_nce", "tchrs_female_other_w_nce",
                 "tchrs_male_pgde", "tchrs_female_pgde", "tchrs_male_b_ed", "tchrs_female_b_ed", 
                  "tchrs_male_other_wo_nce", "tchrs_female_other_wo_nce", "num_sr_staff_male",
                 "num_sr_staff_female", "num_jr_staff_male", "num_jr_staff_female",
                 "days_school_understaffed", "school_max_num_students", "admit_more_num_students",
                 "materials_fee", "uniforms_fee", "sports_fee", "sports_fee_exempt_yn",
                 "num_students_exempt", "in_kind_fees_yn", "booklist_per_class_yn", 
                 "annual_budget_amt_received", "num_tchrs_paid_fed_gov", "num_tchrs_payrl_st_gov",
                 "num_tchrs_othr_payrl_st_gov", "num_tchrs_paid_loc_gov", "num_tchrs_paid_prvt_for_profit", 
                 "num_tchrs_paid_prvt_non_profit", "num_tchrs_paid_other_src", "num_tchrs_no_salary",
                 "num_students_scholarship", "scholarship_amt", "num_library_materials",
                 "times_new_materials_added", "num_classrms_unused", "num_classrooms_multiple_use",
                 "num_other_rooms", "num_sections_pry1", "num_sections_pry2", "num_sections_pry3", 
                 "num_sections_pry4", "num_sections_pry5", "num_sections_pry6", 
                 "num_sections_js1", "num_sections_js2", "num_sections_js3", 
                 "num_attached_benches","num_attached_benches_unused", "num_unattached_benches",
                 "num_unattached_benches_unused", "num_unattached_desks_unused", "num_textbooks_english",
                 "num_textbooks_math", "num_textbooks_social_sci", "num_exercise_books",
                 "num_tchrs_attended_last_day", "grid_months_broken", )


# Creating new variables in 113
edu_113$num_tchrs_male <- apply(cbind(edu_113$num_tchrs_male_full_time, edu_113$num_tchrs_male_part_time), 1,
                                 sum, na.rm=T)

edu_113$num_tchrs_female <- apply(cbind(edu_113$num_tchrs_female_full_time, edu_113$num_tchrs_female_part_time), 1,
                                sum, na.rm=T)

edu_113$num_tchrs_w_nce <- apply(cbind(edu_113$tchrs_male_nce, 
                                       edu_113$tchrs_female_nce, 
                                       edu_113$tchrs_male_other_w_nce,
                                       edu_113$tchrs_female_other_w_nce), 1, sum, na.rm=T)

edu_113$num_benches <- apply(cbind(edu_113$num_attached_benches, 
                                       edu_113$num_unattached_benches), 1, sum, na.rm=T)

edu_113$num_classrms_total <- apply(cbind(edu_113$num_classrms_good_cond, 
                                          edu_113$num_classrms_need_min_repairs, 
                                          edu_113$num_classrms_need_maj_repairs), 1, sum, na.rm=T)

edu_113$ratio_students_to_benches <- replace(edu_113$num_students_total, is.na(edu_113$num_students_total), 0) / 
                                                    replace(edu_113$num_benches, is.na(edu_113$num_benches), 0) 
