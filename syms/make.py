#!/usr/bin/env python

import os
import sys
GAP_COMMAND='gap -A -T -n -q'
# GAP_COMMAND='cat'
THISLIBDIR='/home/ferrario/local/symorb/syms'

gap_script="""
RequirePackage("symorb");
SetInfoLevel(InfoWarning,0); 

WriteAllSyms:=function(action_type,NOB,dim)
local index,all,filename,LSG,allnumber;
all:=AllLSGTK(action_type,NOB,dim);
index:=0;
allnumber:=Length(all);
for LSG in all do
index:=index+1;
Print(" ...processing entry number: ", index, "/",allnumber,"\\n");
filename:=Concatenation("%s/all_", String(action_type), "n", String(NOB), "d", String(dim),"_",String(index) );
MakeMinorbSymFile(filename,LSG);
od;
end;
WriteAllSyms(%i,%i,%i);
"""

NUMBER_OF_BODIES=5
for dim in [2]:
    sys.stdout.write("NOB=%i, dim=%i\n" % (NUMBER_OF_BODIES,dim) ) 
    data_dir="%s/n%id%i" % (THISLIBDIR,NUMBER_OF_BODIES,dim)
    if not os.path.exists(data_dir):
        os.mkdir(data_dir)
        sys.stdout.write("directory %s created\n" % data_dir)
    for action_type in [0,1]:
        sys.stdout.write("*****************************************\ndoing action_type=%i\n******************************************\n" % action_type)
	pipe = os.popen(GAP_COMMAND,'w')
	pipe.write(gap_script % (data_dir,action_type,NUMBER_OF_BODIES,dim))
	failed=pipe.close()
	if failed:
	   sys.exit(failed)
sys.stdout.write("done\n")
sys.exit(0)
