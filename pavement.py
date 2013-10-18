from paver.easy import task, path, sys, sh
import re
import os
import os.path

""" This pavement.py uses the paver library to provide a series of tasks
    that assist in creating a data pipeline from a set of R scripts within
    the directory in which this pavement.py is placed.
"""

ROOT_DIR = os.path.join(os.path.expanduser('~'),
                        'Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning')


def find_R_scripts(base_directory='.'):
    """ Return a list of all the R scripts in given base directory.
    """
    scripts = []
    for root, dirs, files in os.walk(base_directory):
        for f in files:
            filename = os.path.join(root, f)
            if filename.endswith('.R') or filename.endswith('.r'):
                scripts.append(filename)
    #print scripts
    return scripts


def find_reads_and_writes(debug=False):
    """ Goes through all the R scripts in the current directory, and finds
        all the files that are being read and written in these R scripts.
        Output: {
                    script_name: {
                        inputs: [filename1, filename2, ...],
                        outputs: [file1, file2, ...]
                    }, ...
                }
        """
    def file_exists_print_if_not(f, script):
        """ Checks if a file f (read or written by script) exists.
            If file doesn't exist; may mean an error in the script. """
        file_exists = path(f.replace('~', os.path.expanduser('~'))).exists()
        if not file_exists:
            sys.stderr.write("File does not exist: " + f +
                             '; in script ' + script + '\n')
        return file_exists

    def only_existing_files(fnames, script):
        """ Filters a list of files to return only those that exist in fs.
            Uses custom function that prints message on non-existence. """
        return [fname for fname in fnames
                if file_exists_print_if_not(fname, script)]

    def read_excluding_comments(fname, commentchar='#'):
        """ Read a filename (R script) and return its contents, excluding
            any lines that start wit the comment character.  """
        with open(fname, 'r') as f:
            lines = f.readlines()
        comment_exp = re.compile(r'^\s*' + commentchar)
        lines = [line for line in lines if not comment_exp.match(line)]
        return '\n'.join(lines)

    # Functions that read files (filename assumed to be first argument)
    read_functions = ['read\.csv', 'formhubRead', 'load', 'readRDS']
    # Functions that write files (filename assumed to be first argument)
    write_functions = ['write\.csv', 'save', 'saveRDS']
    # Functions that read+write files (filename in first argument is assumed
    #   to be a read operation; second argument is assumed to be a write)
    rw_functions = ['file\.copy']
    rscripts = find_R_scripts()
    deps = {}
    for rscript in rscripts:
        text = read_excluding_comments(rscript)
        deps[rscript] = {'inputs': [], 'outputs': []}
        for rf in read_functions:
            # Find FILENAME in text such as: read.csv("FILENAME"
            # where " and ' are interchangeable
            in_deps = re.findall(rf + r'''\(["']([^'"]*)["']''', text)
            deps[rscript]['inputs'] = deps[rscript]['inputs'] + in_deps
        for wf in write_functions:
            # Find FILENAME in text such as: write.csv(... "FILENAME"
            # where " and ' are interchangeable
            out_deps = re.findall(wf + r'''\([^"']*["']([^"']*)["']''', text)
            deps[rscript]['outputs'] = deps[rscript]['outputs'] + out_deps
        for rwf in rw_functions:
            # Find INF, OUTF in text such as: file.copy("INF", "OUTF", ...
            # where " and ' are interchangeable
            inouts = re.findall(rwf +
                                r'''\(["']([^"']*)["'],\s*["']([^'"]*)["']''',
                                text)
            ins = [inf for inf, outf in inouts]
            outs = [outf for inf, outf in inouts]
            deps[rscript]['inputs'] = deps[rscript]['inputs'] + ins
            deps[rscript]['outputs'] = deps[rscript]['outputs'] + outs
        if debug:
            print ">>> FILE: " + rscript
            print "***IN*** " + " ".join(deps[rscript]['inputs'])
            print "**OUT*** " + " ".join(deps[rscript]['outputs']) + '\n'
    return deps


@task
def make_dependency_graph():
    import pydot
    deps = find_reads_and_writes()
    graph = pydot.Dot(graph_type='digraph', rankdir='LR')
    # create all nodes
    nodes = {}

    def strip(s):
        return s.split('/')[-1]
    for k, v in deps.items():
        k = strip(k)
        nodes[k] = pydot.Node(k, style='filled', fillcolor='red')
        graph.add_node(nodes[k])
        for datafile in v['inputs'] + v['outputs']:
            datafile = strip(datafile)
            if datafile not in nodes.keys():
                nodes[datafile] = pydot.Node(datafile, style='filled',
                                             fillcolor='#5F9F9F')
                graph.add_node(nodes[datafile])
    # create all edges
    for script, v in deps.items():
        script = strip(script)
        for infile in v['inputs']:
            i = strip(infile)
            graph.add_edge(pydot.Edge(nodes[i], nodes[script]))
        for outfile in v['outputs']:
            o = strip(outfile)
            graph.add_edge(pydot.Edge(nodes[script], nodes[o]))
    try:
        graph.write('dependency_graph.pdf', format='pdf')
    except pydot.InvocationException:
        print "You need to install graphviz to create a dependency graph"


@task
def make_makefile(dryrun=False):
    def fix_spaces(names):
        return [name.replace(' ', '\ ') for name in names]
    deps = find_reads_and_writes()
    f = open('Makefile', 'w')
    preamble = """R=R CMD BATCH --no-restore --slave
all:~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/\
in_process_data/nmis/data_774/All_774_LGA.csv
test:
\tRscript tests/*.R > logs/test.log
"""
    f.write(preamble)
    for rscript, v in deps.items():
        inputs = fix_spaces(v['inputs'])
        outputs = fix_spaces(v['outputs'])
        f.write(' '.join(outputs) + ": " + ' '.join(inputs + [rscript]) + '\n')
        f.write('\t$(R) ' + rscript + ' /dev/tty\n')
    f.close()


@task
def make():
    make_makefile()
    sh('make')
