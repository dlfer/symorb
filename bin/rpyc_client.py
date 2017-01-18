#!/usr/bin/env python

import rpyc
import os
import subprocess
PORT_NUMBER=18812
REMOTE_HOST='lkl01'
print "If rpyc_server is not up, exec: "
# os.system("ssh %s python local/symorb/bin/rpyc_server.py" % REMOTE_HOST )
print "ssh %s python local/symorb/bin/rpyc_server.py" % REMOTE_HOST 
#
print "now tunneling the thing..."
os.system("ssh -f -L %i:localhost:%i %s -N" % (PORT_NUMBER,PORT_NUMBER,REMOTE_HOST) ) 

c = rpyc.classic.connect("localhost")

c.execute("x=5")
print c.eval("x+4")
