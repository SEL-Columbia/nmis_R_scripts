R=R CMD BATCH --no-restore
N6=~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/in_process_data/nmis/data_661
N1=~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/in_process_data/nmis/data_113
NP=~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/in_process_data/nmis/data_pilot
N7=~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/in_process_data/nmis/data_774
IN_PROC=~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/in_process_data
RAW_DATA=~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/raw_data
DROPBOX=~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning
n=nmis/nmis_indicators_
e=education
h=health
w=water
f=_facility_level
l=_lga_level
L=_LGA_level
NF=NMIS_Facility.csv
AF=ALL_FACILITY_INDICATORS.csv
#R=echo "\n"; echo
EXT=$(DROPBOX)/external_data

all: $(N7)/Education$(L)_774.csv $(N7)/Health$(L)_774.csv $(N7)/Water$(L)_774.csv $(N7)/Education_774_$(NF) $(N7)/Health_774_$(NF) $(N7)/Water_774_$(NF)
test: tests/GeneralDataChecks.R
	$(R) tests/GeneralDataChecks.R /dev/tty

# external data
$(EXT)/output_data/external_data.csv: base_scripts/external_data.R
	$(R) base_scripts/external_data.R /dev/tty

# merging -- 661
$(IN_PROC)/merged/Education_661_Merged.csv $(IN_PROC)/merged/Health_661_Merged.csv $(IN_PROC)/merged/Water_661_Merged.csv $(IN_PROC)/merged/Local_661_Merged.csv: $(RAW_DATA)/Localities_*.csv $(RAW_DATA)/Water_*.csv $(RAW_DATA)/Health_*.csv $(RAW_DATA)/Education_*.csv base_scripts/MergeDatasets.R 
	$(R) base_scripts/MergeDatasets.R  /dev/tty
# 999 cleaning -- 661
$(IN_PROC)/999cleaned/*_661_999Cleaned.csv: $(IN_PROC)/merged/Education_661_Merged.csv $(IN_PROC)/merged/Health_661_Merged.csv $(IN_PROC)/merged/Local_661_Merged.csv cleaning_999s/clean_out999s.R
	$(R) cleaning_999s/clean_out999s.R  /dev/tty
# outlier cleaning -- 661 (H + E)
$(IN_PROC)/outlier_cleaned/Health_661_outliercleaned.csv $(IN_PROC)/outlier_cleaned/Education_661_outliercleaned.csv: $(IN_PROC)/999cleaned/Health_661_999Cleaned.csv $(IN_PROC)/999cleaned/Education_661_999Cleaned.csv cleaning_outliers/clean_out_outliers.R
	$(R) cleaning_outliers/clean_out_outliers.R /dev/tty
# reclassification -- 661 (W)
$(IN_PROC)/999cleaned/Water_661_999Cleaned_Reclassified.csv: $(IN_PROC)/999cleaned/Water_661_999Cleaned.csv $(IN_PROC)/reclassify_final_148.csv
	$(R) scripts/Water_reclassify_photos.R /dev/tty

# combine to 774 -- health
$(N7)/Education$(L)_774.csv $(N7)/Education_774_$(NF) $(N7)/Health$(L)_774.csv $(N7)/Health_774_$(NF) $(N7)/Water$(L)_774.csv $(N7)/Water_774$(NF): $(N6)/Health_661_$(NF) $(N1)/Health_113_$(NF) $(NP)/Health_Pilot_$(NF) $(N6)/Health$(L)_661.csv $(N1)/Health$(L)_113.csv $(NP)/Health$(L)_Pilot.csv $(N6)/Education_661_$(NF) $(N1)/Education_113_$(NF) $(NP)/Education_Pilot_$(NF) $(N6)/Education$(L)_661.csv $(N1)/Education$(L)_113.csv $(NP)/Education$(L)_Pilot.csv $(N6)/Water_661_$(NF) $(N1)/Water_113_$(NF) $(NP)/Water_Pilot_$(NF) $(N6)/Water$(L)_661.csv $(N1)/Water$(L)_113.csv $(NP)/Water$(L)_Pilot.csv $(EXT)/output_data/external_data.csv $(n)COMBINING.R
	$(R) $(n)COMBINING.R /dev/tty
# lga aggregations -- health -- 661 + 113 + pilot
$(N6)/Health$(L)_661.csv: $(N6)/Health_661_$(AF) $(n)health$(l).R
	$(R) $(n)health$(l).R /dev/tty
$(NP)/Health$(L)_Pilot.csv: $(NP)/Health_Pilot_$(AF) $(n)health$(l)_pilot.R
	$(R) $(n)health$(l)_pilot.R /dev/tty
$(N1)/Health$(L)_113.csv: $(N1)/Health_113_$(AF) $(n)health$(l)_113.R
	$(R) $(n)health$(l)_113.R /dev/tty
# lga aggregations -- education -- 661 + 113 + pilot
$(N6)/Education$(L)_661.csv: $(N6)/Education_661_$(AF) $(n)education$(l).R
	$(R) $(n)education$(l).R /dev/tty
$(NP)/Education$(L)_Pilot.csv: $(NP)/Education_Pilot_$(AF) $(n)education$(l)_pilot.R
	$(R) $(n)education$(l)_pilot.R /dev/tty
$(N1)/Education$(L)_113.csv: $(N1)/Education_113_$(AF) $(n)education$(l)_113.R
	$(R) $(n)education$(l)_113.R /dev/tty
# lga aggregations -- water -- 661 + 113 + pilot
$(N6)/Water$(L)_661.csv: $(N6)/Water_661_$(AF) $(n)water$(l).R
	$(R) $(n)water$(l).R /dev/tty
$(N1)/Water$(L)_113.csv: $(N1)/Water_113_$(AF) $(n)water$(l)_113.R
	$(R) $(n)water$(l)_113.R /dev/tty
$(NP)/Water$(L)_Pilot.csv: $(NP)/Water_Pilot_$(AF) $(n)water$(l)_pilot.R
	$(R) $(n)water$(l)_pilot.R /dev/tty

# facility indicators -- water -- 661
$(N1)/Water_661_$(NF) $(N1)/Water_661_$(AF): $(n)water$(f)_661.R $(IN_PROC)/999cleaned/Water_661_999Cleaned_Reclassified.csv
	$(R) $(n)water$(f)_661.R  /dev/tty
# facility indicators -- water -- 113
$(N1)/Water_113_$(NF) $(N1)/Water_113_$(AF): $(n)water$(f)_113.R $(RAW_DATA)/113/Water_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv
	$(R) $(n)water$(f)_113.R  /dev/tty
# facility indicators -- health -- pilot
$(NP)/Water_Pilot_$(NF) $(NP)/Water_Pilot_$(AF): $(n)water$(f)_pilot.R $(RAW_DATA)/113/Pilot_Water_cleaned_2011Aug29.csv
	$(R) $(n)water$(f)_pilot.R  /dev/tty

# facility indicators -- health -- pilot
$(NP)/Health_Pilot_$(NF) $(NP)/Health_Pilot_$(AF): $(n)health$(f)_pilot.R $(RAW_DATA)/113/Pilot_Data_Health_Clean_2011.11.18.csv
	$(R) $(n)health$(f)_pilot.R  /dev/tty
# facility indicators -- health -- 113
$(N1)/Health_113_$(NF) $(N1)/Health_113_$(AF): $(n)health$(f)_113.R $(RAW_DATA)/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv
	$(R) $(n)health$(f)_113.R  /dev/tty
# facility indicators -- health -- 661
$(N6)/Health_661_$(NF) $(N6)/Health_661_$(AF): $(IN_PROC)/outlier_cleaned/Health_661_outliercleaned.csv $(DROPBOX)/661.csv $(n)health$(f).R
	$(R) $(n)health$(f).R /dev/tty

# facility indicators -- education -- pilot
$(NP)/Education_Pilot_$(NF) $(NP)/Education_Pilot_$(AF): $(n)education$(f)_pilot.R $(RAW_DATA)/113/Pilot_Education_cleaned_2011Nov17.csv 
	$(R) $(n)education$(f)_pilot.R /dev/tty
# facility indicators -- education -- 113
$(N1)/Education_113_$(NF) $(N1)/Education_113_$(AF): $(n)education$(f)_113.R $(RAW_DATA)/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv 
	$(R) $(n)education$(f)_113.R /dev/tty
# facility indicators -- education -- 661
$(N6)/Education_661_$(NF) $(N6)/Education_661_$(AF): $(IN_PROC)/outlier_cleaned/Education_661_outliercleaned.csv $(DROPBOX)/661.csv $(n)education$(f).R
	$(R) $(n)education$(f).R 
