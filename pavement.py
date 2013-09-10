from paver.easy import *
from paver.path import *
import re
from os.path import expanduser, join
import os


ROOT_DIR = join(expanduser('~'), 'Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning')


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
    def only_existing_files(fnames, rscript):
        return [fname for fname in fnames 
                if file_exists_print_if_not(fname, rscript)]
    def read_excluding_comments(fname):
        f = open(fname, 'r')
        lines = f.readlines()
        f.close()
        comment = re.compile(r'^\s*#')
        lines = [line for line in lines if not comment.match(line)]
        return '\n'.join(lines)

    read_functions = ['read\.csv', 'formhubRead', 'save'] # look for first arguments
    write_functions = ['write\.csv', 'load'] # look for second or later arugments
    rw_functions = ['file\.copy']
    rscripts = find_R_scripts()
    deps = {}
    for rscript in rscripts:
        text = read_excluding_comments(rscript)
        deps[rscript] = {'inputs': [], 'outputs': []}
        #import pdb; pdb.set_trace()
        for readf in read_functions:
            candidate_in_deps = only_existing_files(re.findall(readf + r'''\(["']([^'"]*)["']''', text),
                rscript)
            deps[rscript]['inputs'] = deps[rscript]['inputs'] + candidate_in_deps
        for writef in write_functions:
            candidate_out_deps = only_existing_files(re.findall(writef + r'''\([^"]*["']([^"']*)["']''', text),
                rscript)
            deps[rscript]['outputs'] = deps[rscript]['outputs'] + candidate_out_deps
        for rwf in rw_functions:
            inouts = re.findall(rwf + r'''\(["']([^"']*)["'],\s*["']([^'"]*)["']''', text)
            ins = only_existing_files([inf for inf,outf in inouts], rscript)
            outs = only_existing_files([outf for inf,outf in inouts], rscript)
            deps[rscript]['inputs'] = deps[rscript]['inputs'] + ins
            deps[rscript]['outputs'] = deps[rscript]['outputs'] + outs
        if debug:
            print ">>> FILE: " + rscript
            print "***IN*** " + " ".join(deps[rscript]['inputs'])
            print "**OUT*** " + " ".join(deps[rscript]['outputs']) + '\n'
    return deps 

@task
def make_makefile(dryrun=False):
    def fix_spaces(names):
        return [name.replace(' ', '\ ') for name in names]
    deps = find_reads_and_writes()
    f = open('Makefile', 'w')
    preamble = """R=R CMD BATCH --no-restore --slave
all:~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/in_process_data/nmis/data_774/All_774_LGA.csv
test:
\tRscript tests/*.R
"""
    f.write(preamble)
    for rscript,v in deps.items():
        inputs = fix_spaces(v['inputs'])
        outputs = fix_spaces(v['outputs'])
        f.write(' '.join(outputs) + ": " + ' '.join(inputs + [rscript]) + '\n')
        f.write('\t$(R) ' + rscript + ' /dev/tty\n')
    f.close()

@task
def make():
    make_makefile()
    sh('make')

