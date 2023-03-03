#!/usr/bin/python


import sys
sys.path=sys.path+['/home/ferrario/local/symorb/py']
from min_path.mod_minpath import *
# from min_path.mod_remjob import *
import pickle
import readline
import os
import time
from polyray import *




BASE_DIR='/home/ferrario/local/symorb/syms/work'
dirs=[
BASE_DIR + '/n12d2'
]


def make_image(datafile):
  basename, extension =os.path.splitext(datafile)
  PW=PovWorld(background="SkyBlue")
  PW.append(camera(scale=1.0,h_angle=90,v_angle=90))
  PW.append(light_source( position=(-12, -18, 15) , colour="<1,1,1>" ) )
  PW.append(light_source( position=(12, -18, 15) , colour="<1,1,0>") )
  # PW.append(Plane())
  PW.append(PovMinPath(datafile))
  xp=PovRay(Width=600,Height=600,Output_File_Name="%s.ppm" % basename)
  xp.set_host('localhost')
  xp.esegui(PW.data)
  del xp
  del PW
  os.system("convert %s.ppm %s.jpg" % (basename, basename )  )
  os.system("convert %s.ppm -resize 200x200 %s_t.jpg" % (basename, basename )  )
  os.system("rm -f %s.ppm" % basename )






def get_symfiles(dir):
  symfiles=os.listdir(dir)
  result=[]
  for file_i in range(len(symfiles)):
      filename_split=os.path.splitext(symfiles[file_i])
      if filename_split[1] == '.sym' and os.path.exists(dir + "/" + filename_split[0] + '.objs'):
         result = result +  [ dir + '/' + symfiles[file_i] ]
  return result	 

def make_objects(symfile):
  index=0
  basename= os.path.splitext(symfile)[0]
  OBJS_FILENAME=basename+'.objs'
  # x=minpath(symfile)
  # r=remjob(x,200,"new();relax(200)")
  # fd=open(OBJS_FILENAME,'w')
  # pickle.dump(r,fd)
  fd=open(OBJS_FILENAME,'r')
  r = pickle.load(fd)
  fd.close()
  filtered_r=[y for y in r if y.howsol()<0.001]
  for x in filtered_r:
    index += 1
    outfile="%s_%02i.data" % (basename, index)
    x.printsol(outfile)
    sys.stderr.write("solution %02i written to %s...\n" % (index,outfile)  )
    # print x.action()
    try:
      pass
      # make_image(outfile)
    except Exception as v:
      sys.stderr.write("file %s failed! %s \n" % (outfile,v) )
  return

if __name__ == '__main__':
  for dir in dirs:
    sys.stderr.write(" ## processing dir %s...\n" % dir )
    symfiles=get_symfiles(dir)
    for sf in symfiles:
      sys.stderr.write(" ## processing file %s...\n" % sf)
      make_objects(sf)
  sys.exit(0)    
