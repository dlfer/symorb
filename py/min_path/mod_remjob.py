r"""
mod_remjob module

Davide L. Ferrario <ferrario@matapp.unimib.it>

"""
import tempfile
import subprocess
import string
import re
import time
import os
import readline
import pickle
from .myconf import _DISTDIR, SHARED_LOCALDIR
from .mod_minpath import *
import sys
__all__ = ["remjob", "all_omegas", "blob_omegas"]

from min_path.mod_rpyc_minpath import REMOTE_MINORB, remote_home

####


def mycmp(self, other):
    if self[1] < other[1]:
        return -1
    elif self[1] == other[1]:
        return 0
    elif self[1] > other[1]:
        return 1
####


def mysort(lista):
    result = []
    lista.sort(mycmp)
    for i in lista:
        if (result == []) or (i[1] > result[-1][1] + 10**(-3)):
            result = result + [i]
    return result

####


def parload(nomefile):
    fd = open(nomefile)  # .objs
    parlist = pickle.load(fd)
    newlist = []
    for it in parlist:
        newlist = newlist + [[it, it.a]]
    newlist = mysort(newlist)
    result = []
    for it in newlist:
        result = result + [it[0]]
    return result
####

# -----------------------------------------------------------


def non_va_remjob(x, nsol, todo):
    if REMOTE_MINORB:
        return remote_remjob(x, nsol, todo)
    else:
        return local_remjob(x, nsol, todo)


# -----------------------------------------------------------

def remjob(x, NUMBEROFSOLUTIONS, todo):
    from .myconf import _DISTDIR, SHARED_LOCALDIR
    sys.stderr.write("remjob called with nsol= %s\n" % NUMBEROFSOLUTIONS)
    todo_list = string.split(todo, ';')
    if type(x) != type([]):
        x.TODOLIST = todo_list
    else:
        for y in x:
            y.TODOLIST = todo_list
    sys.stderr.write("beginning the job...\n")
    if REMOTE_MINORB:
        remhome = remote_home()
        _DISTDIR = remhome + "/local/symorb"
        SHARED_LOCALDIR = remhome+"/.symorb"
    symfilename = ''
    _PAR_COMMAND = _DISTDIR + """/py/par/parminpath --solutions=%i --output=%s  --load=%s """
    # _PAR_COMMAND= _DISTDIR + """/bin/parminpath_launch.sh --solutions=%i --output=%s  --load=%s """
    _TMP_OBJS_FILE = os.path.join(SHARED_LOCALDIR, "objsfile_%i%i%i.objs" % (
        os.geteuid(), os.getpid(), int(time.time())))  # __HERE__ TODO
    _TMP_OBJ = os.path.join(SHARED_LOCALDIR, "obj_%i%i%i.obj" % (
        os.geteuid(), os.getpid(), int(time.time())))
    tmpobj_fd = open(tempfile.mkstemp()[1], 'w')
    # tmpobj_fd=open(_TMP_OBJ,'w')
    pickle.dump(x, tmpobj_fd)
    tmpobj_fd.close()
    os.system("scp %s lkl01:%s" % (tmpobj_fd.name, _TMP_OBJ))
    os.unlink(tmpobj_fd.name)
    # os.system(_SCP_COMMAND % (_TMP_OBJ , _TMP_OBJ ))
    sys.stderr.write(":: about to exec the following...:\n")
    # DP=subprocess.Popen("echo DP",shell=True,stdout=subprocess.PIPE)
    # print DP.communicate()
    sys.stderr.write(_PAR_COMMAND %
                     (NUMBEROFSOLUTIONS, _TMP_OBJS_FILE, _TMP_OBJ))
    # os.system(_PAR_COMMAND  % ( NUMBEROFSOLUTIONS,  _TMP_OBJS_FILE , _TMP_OBJ ) )
    # DP=subprocess.Popen( _PAR_COMMAND  % ( NUMBEROFSOLUTIONS,  _TMP_OBJS_FILE , _TMP_OBJ )  , shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # here local/remote diff
    if REMOTE_MINORB:
        DP = subprocess.Popen("ssh lkl01 " + _PAR_COMMAND %
                              (NUMBEROFSOLUTIONS,  _TMP_OBJS_FILE, _TMP_OBJ), shell=True)
        # os.system( "ssh lkl01 " +  _PAR_COMMAND  % ( NUMBEROFSOLUTIONS,  _TMP_OBJS_FILE , _TMP_OBJ ) )
    else:
        DP = subprocess.Popen(_PAR_COMMAND % (
            NUMBEROFSOLUTIONS,  _TMP_OBJS_FILE, _TMP_OBJ), shell=True)
    DP.communicate()
    sys.stderr.write(":: did it work? [%s] ...:\n" % str(os.uname()))
    resultfd_name = tempfile.mkstemp()[1]
    os.system("scp lkl01:%s %s" % (_TMP_OBJS_FILE, resultfd_name))
    result = parload(resultfd_name)
    os.unlink(resultfd_name)
    if REMOTE_MINORB:
        os.system("ssh lkl01 rm -f %s" % _TMP_OBJS_FILE)
        os.system("ssh lkl01 rm -f %s" % _TMP_OBJ)
    else:
        os.system("rm -f %s" % _TMP_OBJS_FILE)
        os.system("rm -f %s" % _TMP_OBJ)

    sys.stderr.write(
        "************************************************************\n...done!\n")
    sys.stderr.write("""
...found %i solutions:
""" % len(result))
    for i in range(len(result)):
        result[i].symfilename = symfilename
        sys.stderr.write("sol %i:  action=%f   howsol=%f\n" %
                         (i, result[i].a, result[i].h))
    return result


def all_omegas(x, omega_int, steps):
    res = ""
    x.omega = [0.0, 0.0, omega_int[0]]
    remres = remjob(x, 100, "new();relax(2)")
    for i in range(steps+1):
        om = omega_int[0] + \
            float(i) * (omega_int[1] - omega_int[0]) / float(steps)
        res = res + "%f " % om
        for j in range(len(remres)):
            remres[j].omega = [0.0, 0.0, om]
            remres[j].relax(2)
            # remres=remjob(x,24,"new();relax(2)")
            res = res + "%f " % remres[j].a
        res = res + "\n"
    return res


def blob_omegas(x, omega_int, steps):
    res = ""
    for i in range(steps+1):
        om = omega_int[0] + \
            float(i) * (omega_int[1] - omega_int[0]) / float(steps)
        res = res + "%f " % om
        x.omega = [0, 0, om]
        remres = remjob(x, 100, "new();relax(2)")
        for y in remres:
            res = res + "%f " % y.a
        res = res + "\n"
    return res
