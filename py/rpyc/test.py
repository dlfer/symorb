#!/usr/bin/env python

import rpyc
import os
import subprocess
PORT_NUMBER=18812
REMOTE_HOST='lkl01'
try:
 os.system("ssh lkl01 python /usr/lib64/python2.6/site-packages/rpyc/servers/classic_server.py")
except Exception, v:
 sys.stderr.wrote("Error: %s\n" % v )
os.system(" echo ssh -f -L %i:localhost:%i %s -N" % (PORT_NUMBER,PORT_NUMBER,REMOTE_HOST) ) 

c = rpyc.classic.connect("localhost")

c.execute("x=5")
print c.eval("x+4")
