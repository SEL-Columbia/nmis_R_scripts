from paver.easy import *
from paver.path import *
import re
from os.path import expanduser, join
import os


ROOT_DIR = join(expanduser('~'), 'Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning')


def nmis_datafile(effort, datafiletype, sector):
    """ Return name of datafile, depending on effort (pilot, 113, 661, 774), 
            lga or facility level (lga, nmis, all) and sector (education, health, water)"""
    if effort not in ['pilot', '113', '661', '774'] or sector not in ['education', 'health', 'water'] or \
                    datafiletype not in ['lga', 'nmis', 'all']: raise Exception('incorrect arguments supplied')
    dirname = '/'.join([ROOT_DIR, 'in_process_data', 'nmis', 'data_' + effort])
    tmp = {'lga': 'LGA_level', 'nmis': 'NMIS_Facility', 'all': 'ALL_FACILITY_INDICATORS'}[datafiletype]
    if datafiletype == "lga":
        filename = '_'.join([sector.capitalize(), tmp, effort + '.csv'])
    else:
        filename = '_'.join([sector.capitalize(), effort, tmp + '.csv'])
    return '/'.join([dirname, filename])


def find_R_scripts(path='.'):
    """ find all the R scripts in this folder"""
    s = []
    for root, dirs, files in os.walk(path):
        for f in files:
            filename = os.path.join(root, f)
            if filename.endswith('.R') or filename.endswith('.r'):
                s.append(filename)
    #print s
    return s

@task
def find_reads_and_writes():
    read_functions = ['read.csv', 'formhubRead']
    write_functions = ['write.csv']
    rscripts = find_R_scripts()
    reads = {}
    writes = {}
    for rscript in rscripts:
        f = open(rscript, 'r')
        text = f.read()
        reads[rscript] = []
        writes[rscript] = []
        for readf in read_functions:
            reads[rscript] = reads[rscript] + re.findall(readf + r'\("([^"]*)"', text)
        for writef in write_functions:
            writes[rscript] = writes[rscript] + re.findall(writef + r'\([^"]*"([^"]*)"', text)
        f.close()

        print "For rscript " + rscript
        print "IN " + " ".join(reads[rscript]),
        print "OUT " + " ".join(writes[rscript]) + '\n'


@task
def combine():
    """Combines all the final files into the 774 files"""
    sources = [nmis_datafile(effort, lorf, sec) for effort in ['113', '661', 'pilot']
               for lorf in ['lga', 'nmis'] for sec in ['education', 'water', 'health']]
    paths = [path(source) for source in sources]
    print paths
    print [p.mtime for p in paths]


@task
def grovel():
    """Grovel a little"""
    print "What can I do for you, Master?"


@task
@needs(('hello', 'grovel'))
def say_all():
    """Say hello, then grovel some"""
    print "This should be at the end"
