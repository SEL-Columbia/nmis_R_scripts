import json
import csv

def nmis_id_json_parser(infile):
    """
    takes a file, parse the json file by line
    return the uuid pairs of each facility
    """
    my_dat = my_file.readlines()
    id_list = []
    for element in my_dat:
        current = json.loads(element)
        nmis_uuid = current["long_id"]
        if current.has_key("matched"):
            fcl_uuid = current["lga"]["long_id"]
            fcl_facility_name = current["lga"]["facility_name"]
            fcl_community = current["lga"]["community"]
            fcl_ward = current["lga"]["ward"]
        else:
            fcl_uuid = None
            fcl_facility_name = None
            fcl_community = None
            fcl_ward = None
        obj = {"nmis_uuid": nmis_uuid,
               "fcl_uuid": fcl_uuid,
               "fcl_facility_name":fcl_facility_name,
               "fcl_community":fcl_community,
               "fcl_ward":fcl_ward}
        id_list.append(obj)
    return id_list

my_file = open('~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result/jsondumps/nmis_edu.json')
id_list = nmis_id_json_parser(my_file)

out_file = open("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result/baseline_matching_education.csv", "w")
writer = csv.DictWriter(out_file, fieldnames = id_list[0].keys())
writer.writeheader()
writer.writerows(id_list)
