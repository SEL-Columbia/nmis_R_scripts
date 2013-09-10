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
def find_reads_and_writes(debug=False):
    def file_exists_print_if_not(f, script):
        fileexists = path(f.replace('~', expanduser('~'))).exists()
        if not fileexists: sys.stderr.write("File does not exist: " + f + '; in script ' + script + '\n')
        return fileexists
    def read_excluding_comments(fname):
        f = open(fname, 'r')
        lines = f.readlines()
        f.close()
        comment = re.compile(r'^\s*#')
        lines = [line for line in lines if not comment.match(line)]
        return '\n'.join(lines)

    read_functions = ['read.csv', 'formhubRead', 'file.copy'] # look for first arguments
    write_functions = ['write.csv', 'file.copy'] # look for second or later arugments
    rscripts = find_R_scripts()
    deps = {}
    for rscript in rscripts:
        text = read_excluding_comments(rscript)
        deps[rscript] = {'inputs': [], 'outputs': []}
        #import pdb; pdb.set_trace()
        for readf in read_functions:
            candidate_in_deps = re.findall(readf + r'\("([^"]*)"', text)
            candidate_in_deps = [filename for filename in candidate_in_deps
                if file_exists_print_if_not(filename, rscript)]
            deps[rscript]['inputs'] = deps[rscript]['inputs'] + candidate_in_deps
        for writef in write_functions:
            candidate_out_deps = re.findall(writef + r'\([^"]*"([^"]*)"', text)
            candidate_out_deps = [filename for filename in candidate_out_deps
                if file_exists_print_if_not(filename, rscript)]
            deps[rscript]['outputs'] = deps[rscript]['outputs'] + candidate_out_deps
        if debug:
            print ">>> FILE: " + rscript
            print "***IN*** " + " ".join(deps[rscript]['inputs'])
            print "**OUT*** " + " ".join(deps[rscript]['outputs']) + '\n'
    return deps 

@task
def make_makefile():
    def fix_spaces(names):
        return [name.replace(' ', '\ ') for name in names]
    deps = find_reads_and_writes()
    f = open('Makefile', 'w')
    for rscript,v in deps.items():
        inputs = fix_spaces(v['inputs'])
        outputs = fix_spaces(v['outputs'])
        f.write(' '.join(outputs) + ": " + ' '.join(inputs + [rscript]) + '\n')
        f.write('\tR CMD BATCH --slave --no-restore ' + rscript + ' /dev/tty\n')
    f.close()

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
