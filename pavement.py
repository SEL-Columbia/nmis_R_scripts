from paver.easy import task, sh
import re
import os
import os.path

""" This pavement.py uses the paver library to provide a series of tasks
    that assist in creating a data pipeline from a set of R scripts within
    the directory in which this pavement.py is placed.
"""


def find_r_scripts(base_directory='.'):
    """ Return a list of all the R scripts in given base directory.
    """
    scripts = []
    for root, _, files in os.walk(base_directory):
        for my_file in files:
            filename = os.path.join(root, my_file)
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
    def read_excluding_comments(fname, commentchar='#'):
        """ Read a filename (R script) and return its contents, excluding
            any lines that start wit the comment character.  """
        with open(fname, 'r') as f:
            lines = f.readlines()
        comment_exp = re.compile(r'^\s*' + commentchar)
        lines = [line for line in lines if not comment_exp.match(line)]
        return '\n'.join(lines)

    # Functions that read files (filename assumed to be first argument)
    read_functions = [r'read.csv', r'formhubRead', r'load', r'readRDS']
    # Functions that write files (filename assumed to be first argument)
    write_functions = [r'write.csv', r'save', r'saveRDS']
    # Functions that read+write files (filename in first argument is assumed
    #   to be a read operation; second argument is assumed to be a write)
    rw_functions = [r'file.copy']
    rscripts = find_r_scripts()
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
def make_dependency_graph(filename='dependency_graph', ext='pdf'):
    """ Goes through all the R scripts in the current directory, finds all
    \t\t\t files being read and written, creates a dependency graph, & outputs
    \t\t\t it to filename.ext (given that ext is supported by graphviz)
    \t\t\t Requires: graphviz to be installed, python module pydot. """
    import pydot
    deps = find_reads_and_writes()
    # The pydot graph which we will re-encode the dependencies (deps) into
    graph = pydot.Dot(graph_type='digraph', rankdir='LR')
    # The nodes dict makes sure that each filename is added only once to graph
    nodes = {}

    def strip(s):
        """ Takes a string like x/y/z/abc.def and returns abc.def """
        return s.split('/')[-1]

    # Pass 1: add all nodes
    for k, v in deps.items():
        k = strip(k)  # k is script name, stripped to just filename
        nodes[k] = pydot.Node(k, style='filled', fillcolor='red')
        graph.add_node(nodes[k])
        for datafile in v['inputs'] + v['outputs']:
            datafile = strip(datafile)
            if datafile not in nodes.keys():
                nodes[datafile] = pydot.Node(datafile, style='filled',
                                             fillcolor='#5F9F9F')
                graph.add_node(nodes[datafile])

    # Pass 2: add all edges
    for script, v in deps.items():
        script = strip(script)
        for infile in v['inputs']:
            i = strip(infile)
            graph.add_edge(pydot.Edge(nodes[i], nodes[script]))
        for outfile in v['outputs']:
            o = strip(outfile)
            graph.add_edge(pydot.Edge(nodes[script], nodes[o]))
    try:
        filename = "%s.%s" % (filename, ext)
        graph.write(filename, format=ext)
    except pydot.InvocationException:  # happens if graphviz isn't installed
        print "You need to install graphviz to create a dependency graph"


@task
def make_makefile():
    """ Goes through all the R scripts in the current directory, finds all
    \t\t\t files being read and written, and writes out makefile.
    \t\t\t Given RSCRIPT that has infiles INF1, INF2 and outfiles OUTF1, OUTF2
    \t\t\t the Makefile will have a rule like the below (with proper indents).

    \t\t\t OUTF1 OUTF2: INF1 INF2 RSCRIPT
    \t\t\t     $(R) RSCRIPT /dev/tty
    """
    def escape_spaces(names):
        """ Replace space ( ) with escaped space(\ ) in a list of strings """
        return [name.replace(' ', '\ ') for name in names]
    preamble = """\
R=R CMD BATCH --no-restore --slave
all:~/Dropbox/Nigeria/Nigeria\ 661\ Baseline\ Data\ Cleaning/\
in_process_data/nmis/data_774/All_774_LGA.csv
test:
\tRscript tests/*.R > logs/test.log
"""

    deps = find_reads_and_writes()
    with open('Makefile', 'w') as f:
        f.write(preamble)
        for rscript, v in deps.items():
            inputs = escape_spaces(v['inputs'])
            outputs = escape_spaces(v['outputs'])
            f.write(' '.join(outputs) + ": " +
                    ' '.join(inputs + [rscript]) + '\n')
            f.write('\t$(R) ' + rscript + ' /dev/tty\n')


@task
def make():
    """ Create a new makefile and run make."""
    make_makefile()
    sh('make')
