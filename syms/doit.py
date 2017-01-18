#!/usr/bin/python


import sys
sys.path=sys.path+['/home/ferrario/local/symorb/py']
from min_path.mod_minpath import *
from min_path.mod_remjob import *
import pickle
import readline
import os
import time

BASE_DIR='/home/ferrario/local/symorb/syms'
dirs=[
BASE_DIR + '/n3d2'
]

def get_symfiles(dir):
  symfiles=os.listdir(dir)
  result=[]
  for file_i in range(len(symfiles)):
      if os.path.splitext(symfiles[file_i])[1] == '.sym':
         result = result +  [ dir + '/' + symfiles[file_i] ]
  return result	 

def make_objects(symfile):
  basename= os.path.splitext(symfile)[0]
  OBJS_FILENAME=basename+'.objs'
  x=minpath(symfile)
  r=remjob(x,200)
  fd=open(OBJS_FILENAME,'w')
  pickle.dump(r,fd)
  sys.stderr.write("file %s created...\n" % OBJS_FILENAME)
  fd.close()
  return

if __name__ == '__main__':
  for dir in dirs:
    sys.stderr.write(" ## processing dir %s...\n" % dir )
    symfiles=get_symfiles(dir)
    for sf in symfiles:
      sys.stderr.write(" ## processing file %s...\n" % sf)
      make_objects(sf)
  sys.exit(0)    
