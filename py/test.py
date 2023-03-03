#!/usr/bin/python

r"""
minpath Class:

Davide L. Ferrario <ferrario@matapp.unimib.it>

"""
import sys
sys.path=sys.path+['/usr/local/gap4r3/pkg/symorb/py']
from min_path.mod_minpath import *
from min_path.mod_remjob import *
from min_path.syms import *

if __name__ == "__main__":
    x=eight_c6
    res=remjob(x,12,"new();relax(2)")

