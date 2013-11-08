import json
import csv
import os


ROOT_DIR = os.path.join(os.path.expanduser('~'), 'Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result')

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
            try:
                fcl_uuid = current["lga"]["long_id"]
                fcl_facility_name = current["lga"]["facility_name"]
                fcl_community = current["lga"]["community"]
                fcl_ward = current["lga"]["ward"]
            except Exception, e:
                print str(e)
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



def run(input_file, output_file):
    id_list = nmis_id_json_parser(input_file)
    writer = csv.DictWriter(output_file, fieldnames = id_list[0].keys())
    writer.writeheader()
    writer.writerows(id_list)
    input_file.close()
    output_file.close()

def my_file_path(relative_path, action='r'):
    my_file = os.path.join(ROOT_DIR, relative_path)
    return open(my_file, action)


my_file = my_file_path('jsondumps/nmis_edu.json')
out_file = my_file_path("baseline_matching_education.csv", "w")
try:
    run(my_file, out_file)
except Exception,e:
    print str(e)

my_file = my_file_path('jsondumps/nmis_health.json')
out_file = my_file_path("baseline_matching_health.csv", "w")
try:
    run(my_file, out_file)
except Exception,e:
    print str(e)

