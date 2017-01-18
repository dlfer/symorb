#!/usr/bin/env python
"""
DP XVI
"""

import sys
import os
import hashlib
import rpyc
from rpyc.utils.server import ThreadedServer 
from rpyc.utils.registry import REGISTRY_PORT
from rpyc.utils.registry import UDPRegistryClient, TCPRegistryClient
from rpyc.utils.authenticators import AuthenticationError
from rpyc.core import SlaveService
HOST='localhost'
SERVER_PORT=0
LOGFILE='/tmp/minpath.log'

md= hashlib.md5(os.urandom(32))
SharedToken=md.hexdigest() 
SharedTokenLength=2* md.digest_size 


SymorbDir=os.path.expanduser('~/.symorb')
if not os.path.exists(SymorbDir):
  os.makedirs(SymorbDir)
  sys.stderr.write("Directory %s created!\n" % SymorbDir )

RUNfileName=os.path.join(SymorbDir, "rpyc_server-%i" % os.getpid() ) 
RunFile=file(RUNfileName,'w')

## __HERE__ TODO: generate one-time-random password
## something in ${HOME}/.symorb

def TokenAuthenticator(sock):
 # greeting = SharedToken #where to get any data from the connection? 
 greeting = sock.recv(SharedTokenLength)
 if greeting != SharedToken:
    raise AuthenticationError("Wrong SharedToken!")
 return sock, None 

# class MySharedService(SlaveService):
#  def __init__(self, conn):
#    global SharedToken
#    self._conn = conn
#    self.SharedToken=SharedToken


def serve_threaded():
    global SERVER_PORT, LOGFILE
    REGISTRAR = UDPRegistryClient(ip = '255.255.255.255', port = REGISTRY_PORT )
    t = ThreadedServer(SlaveService, hostname = HOST, 
        port = SERVER_PORT, reuse_addr = True, 
        authenticator = TokenAuthenticator, registrar = REGISTRAR ,
        auto_register = False )
    t.logger.quiet = False # options.quiet
    t.logger.console = open(LOGFILE, "a")
    sys.stderr.write("__HIDDEN__DATA__:%s:%s\n" %  (t.port, SharedToken) )
    RunFile.write("%s:%s\n" % (t.port, SharedToken) )
    RunFile.close()
    t.start()


if __name__ == "__main__":
    serve_threaded()


