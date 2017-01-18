import os
import sys

if 'SYMORBDIR' in os.environ: 
  _DISTDIR=os.environ['SYMORBDIR']
else:
  _DISTDIR=os.environ['HOME'] + "/local/symorb"

if ('REMOTE_MINORB' in os.environ) or (sys.platform == 'darwin'):
 # use remote minorb.bin
 REMOTE_MINORB=True
else:
 REMOTE_MINORB=False


_MINORB_PATH=_DISTDIR+"/f90/minorb4/minorb.bin"
_MINORB_PATH_WITHCOLL=_DISTDIR+"/f90/minorb4_withcoll/minorb.bin"
_ORBVIEW_PATH=_DISTDIR+"/bin/orbview4 --cs=0 "
_DATA_DIR=_DISTDIR+'/syms'

SHARED_LOCALDIR=os.path.expanduser("~/.symorb")
NUM_PARNODES=32
HostFile=os.path.join(_DISTDIR,'OpenMpi-Hostfile')

