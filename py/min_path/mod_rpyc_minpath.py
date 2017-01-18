r"""
rpyc minorb_bin method

Davide L. Ferrario <ferrario@matapp.unimib.it>

"""

from myconf import _MINORB_PATH,_ORBVIEW_PATH,_MINORB_PATH_WITHCOLL
from myconf import REMOTE_MINORB

import sys
import rpyc
import re
import subprocess

from rpyc.utils import * 
from rpyc import Connection, SocketStream, Channel, SlaveService
from subprocess import PIPE

__all__=['_minorb_bin','_minorb_bin_withcoll','_strip_comments','clean_remote_processes','start_remote_server','token_connect','remote_home']

# RemoteServer_porta=None
# SharedToken=None
# they should be initialized!!!

def connector_to_rpyc(data,THIS_PATH):
  # sys.stderr.write("to connect to %s\n" % THIS_PATH)
  # (cout,cin) = popen2.popen2(THIS_PATH)
  #__ c = rpyc.classic.connect("localhost")
  c=token_connect("localhost")
  c.modules.sys.path.append(c.modules.os.path.expanduser('~/local/symorb/py'))
  # c.modules.sys.path.append('/home/ferrario/local/symorb/py') ##__TODO__
  PIPE=c.modules.subprocess.PIPE
  # PP = c.modules.subprocess.Popen(THIS_PATH,shell=True,bufsize=8192,stdin=PIPE,stdout=PIPE, close_fds=True)
  TODO_PATH=eval('c.modules.min_path.myconf.' + THIS_PATH)
  LD_PRELOAD='LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${HOME}/local/symorb/lf9562-lib'
  # sys.stderr.write("to connect to %s\n" % TODO_PATH)
  PP = c.modules.subprocess.Popen(LD_PRELOAD + " " + TODO_PATH,shell=True,bufsize=8192,stdin=PIPE,stdout=PIPE, stderr=PIPE, close_fds=True)
  c.modules.sys.stderr=sys.stderr
  # PP = c.modules.subprocess.Popen(TODO_PATH,stdin=PIPE,stdout=PIPE,stderr=PIPE)
  # (cin,cout) = (PP.stdin,PP.stdout)
  data=_strip_comments(data)
  res,err= PP.communicate(input=data)
  #cin.write(data); 
  #cin.close()
  # res=PP.stdout.read(); 
  #cout.close()
  # sys.stderr.write(PP.stderr.read())
  sys.stderr.write(err)
  if len(res)<4:
    sys.stderr.write("FATAL ERROR: minorb broken...\n")
    raise Exception("minorb did not respond correctly..\n")
  return res

def connector_to_minorb(data,THIS_PATH):
  # sys.stderr.write("to connect to %s\n" % THIS_PATH)
  # (cout,cin) = popen2.popen2(THIS_PATH)
  PIPE=subprocess.PIPE
  TODO_PATH=eval(THIS_PATH)
  PP = subprocess.Popen(TODO_PATH,shell=True,bufsize=8192,stdin=PIPE,stdout=PIPE, stderr=PIPE, close_fds=True)
  data=_strip_comments(data)
  res,err= PP.communicate(input=data)
  sys.stderr.write(err)
  if len(res)<4:
    sys.stderr.write("FATAL ERROR: minorb broken...\n")
    raise Exception("minorb did not respond correctly..\n")
  return res

def _strip_comments(adata):
                reg=re.compile(r"!.*",re.M)
                adata=re.sub(reg,'',adata)
                reg=re.compile("^ *\n",re.M)
                adata=re.sub(reg,'',adata)
                return adata

def _minorb_bin(data):
 if REMOTE_MINORB:
  return connector_to_rpyc(data,'_MINORB_PATH')
 else:
  return connector_to_minorb(data,'_MINORB_PATH')

def _minorb_bin_withcoll(data):
 if REMOTE_MINORB:
  return connector_to_rpyc(data,'_MINORB_PATH_WITHCOLL')
 else:
  return connector_to_minorb(data,'_MINORB_PATH')



#===================

def get_remote_processes(remote_host):
  QQ = subprocess.Popen("ssh  %s ls %s" % (remote_host,".symorb/rpyc_server*"), shell=True, bufsize=8192,stdout=PIPE,stderr=PIPE)
  res,err = QQ.communicate()
  result = []
  for x in res.split("\n"):
    if x[:4] == ".sym":
      result += [ x.split('-')[-1] ] 
  return result    

def clean_remote_processes(remote_host):
 for x in get_remote_processes(remote_host):
  QQ = subprocess.Popen("ssh  %s rm -f .symorb/rpyc_server-%s" % (remote_host,x), shell=True, bufsize=8192,stdout=PIPE,stderr=PIPE)
  res,err = QQ.communicate()
  QQ = subprocess.Popen("ssh  %s kill %s" % (remote_host,x), shell=True, bufsize=8192,stdout=PIPE,stderr=PIPE)
  res,err = QQ.communicate()
  

def start_remote_server(remote_host):
  START_COMMAND = [ "ssh", remote_host +  " /home/ferrario/local/symorb/bin/rpyc_server.sh " ] 
  # START_COMMAND="ssh %s python ~/local/symorb/bin/rpyc_server.py" % remote_host
  # pipe = subprocess.Popen(START_COMMAND, shell=True, bufsize=8192, stdout=PIPE ).stdout
  PP = subprocess.Popen(" ".join(START_COMMAND), shell=True, bufsize=8192, stdout=PIPE,stderr=PIPE)
  res,err= PP.communicate()
  res = res.strip()
  port_number,shared_token= res.split(":")
  print  "(=> running remote server on %s:%s ...)" % (remote_host,port_number)
  return (port_number, shared_token)


def TokenConnect(host, port, service = rpyc.SlaveService, config = {}):
    global SharedToken
    s = SocketStream.connect(host, port)
    # s.sock.send(SharedToken)
    s.write(SharedToken)
    return Connection(service, Channel(s), config = config)


def token_connect(host):
 # RemoteServer_porta,SharedToken=start_remote_server(host)
 return TokenConnect('localhost', int(RemoteServer_porta) , SlaveService)


#------------------------------------------------------------------------

def remote_remjob(x,nsol,todo):
  sys.stderr.write("trying ... \n")
  c=token_connect("localhost")
  c.modules.sys.path.append('/home/ferrario/local/symorb/py') ##__TODO__
  c.modules.sys.stderr=sys.stderr
  c.modules.sys.stdout=sys.stdout
  c.modules.sys.stdin=sys.stdin
  # with rpyc.classic.redirected_stdio(c):
  result= c.modules.min_path.mod_remjob.local_remjob(x,nsol,todo)
  return result

def remote_home():
  c=token_connect("localhost")
  return c.modules.os.path.expanduser('~')





#------------------------------------------------------------------------
if __name__=='__main__':
 REMOTE_HOST='lkl01'
 clean_remote_processes(REMOTE_HOST)
 # RemoteServer_porta,SharedToken=start_remote_server(REMOTE_HOST)
 #c = TokenConnect('localhost', int(RemoteServer_porta) , SlaveService)
 #c.execute("x=5")
 #print c.eval("x+4")
