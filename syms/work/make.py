#!/usr/bin/env python

import os
import sys
GAP_COMMAND='gap -A -T -n -q'
# GAP_COMMAND='cat'
THISLIBDIR='/home/ferrario/local/symorb/syms/work'

gap_script="""
RequirePackage("symorb");
SetInfoLevel(InfoWarning,3); 

MkOrbs(10000,"%s",%i,%i,%i);
"""

NUMBER_OF_BODIES=12
ORDER=12

for dim in [2]:
    sys.stdout.write("NOB=%i, dim=%i\n" % (NUMBER_OF_BODIES,dim) ) 
    data_dir="%s/n%id%i" % (THISLIBDIR,NUMBER_OF_BODIES,dim)
    if not os.path.exists(data_dir):
        os.mkdir(data_dir)
        sys.stdout.write("directory %s created\n" % data_dir)
    pipe = os.popen(GAP_COMMAND,'w')
    pipe.write(gap_script % (data_dir,ORDER,NUMBER_OF_BODIES,dim))
    failed=pipe.close()
    if failed:
	   sys.exit(failed)
sys.stdout.write("done\n")
sys.exit(0)
