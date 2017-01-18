r"""
minpath Class:

Davide L. Ferrario <ferrario@matapp.unimib.it>

"""

from myconf import _MINORB_PATH,_ORBVIEW_PATH,_MINORB_PATH_WITHCOLL

import sys
from min_path.mod_rpyc_minpath import *
import re
import string
import os
import time
import pickle
import copy

__all__=["minpath","scan_files"]

_CONFIG_STRING="""!! CONFIG STRING
&CONFIG
COMMAND="%s"
SPMETHOD=%i
ALG=%i
steps=%i
ALPHA=%.24e
/
%.24e !! omega(1)
%.24e !! omega(2)
%.24e !! omega(3)
"""

_TMPFILE_NAME="/tmp/minpath_%i%i%i" % (os.geteuid(),os.getpid(),int(time.time())) 

#def _strip_comments(adata):
#		reg=re.compile(r"!.*",re.M)
#		adata=re.sub(reg,'',adata)
#		reg=re.compile("^ *\n",re.M)
#		adata=re.sub(reg,'',adata)
#		return adata	
#
#def _minorb_bin(data):
#  (cout,cin) = popen2.popen2(_MINORB_PATH)
#  data=_strip_comments(data)
#  cin.write(data); cin.close()
#  res=cout.read(); cout.close()
#  if len(res)<4:
#    sys.stderr.write("FATAL ERROR: minorb broken...\n")
#    raise Exception("minorb did not respond correctly..\n")
#  return res
#
#def _minorb_bin_withcoll(data):
#  (cout,cin) = popen2.popen2(_MINORB_PATH_WITHCOLL)
#  data=_strip_comments(data)
#  cin.write(data); cin.close()
#  res=cout.read(); cout.close()
#  if len(res)<4:
#    sys.stderr.write("FATAL ERROR: minorb broken...\n")
#    raise Exception("minorb did not respond correctly..\n")
#  return res
#
_endofchunk=':\n'

class minpath:
	algs="""
0:	Unconstrained Minimization with analytic Gradient
1:	Unconstrained Minimization with Analytic Hessian
2:	Unconstrained Minimization with finite-Difference Hessian
3:	Unconstrained Minimization with Conjugate Gradient and analytic Gradient
4:	Unconstrained Minimization without gradient (nonsmooth)
5:	Linearly Constrained Minimization with Analytic Gradient
6:	Step-Flow Descent
7:	Simple Conjugate Gradient
------------------------------------------------------------------------
100:	Newton-Powell Finite-difference Jacobian
200:	Newton-Powell Analytic Jacobian
300:	Secant Broyden's Update and Finite-difference Jacobian
400:	Secant Broyden's Update and Analytic Jacobian
"""	
	def __init__(self,*args):
		if len(args)==1:
			symfilename=args[0]
		elif len(args)==0:
			symfilename=self.getsymfilename('.')
			if symfilename=="":
				self.invalid=1
				return None
		else:
			print "usage: x=minpath( *symfilename )"
			self.invalid=1
			return None
		self.symfilename=symfilename
		try:
			fd=open(symfilename,'r')
			self.symdata=fd.read()
			fd.close()
		except:
			sys.stdout.write("No file %s found\n" % symfilename)
			self.invalid=1
			return None
		self.invalid=0
		self.withcoll=0
		self.confdata=""
		self.viewfile=""
		headers = string.split(_strip_comments(self.symdata),'\n')[0:3]
		for i in range(len(headers)):
			headers[i] = re.compile(r"^ *").sub('',headers[i]) 
		self.NOB=string.atoi(string.split(headers[1],' ')[0])
		self.dim=string.atoi(string.split(headers[2],' ')[0])
		self.masses=range(self.NOB)
		self.massdata=""
		for i in range(self.NOB):
			self.masses[i]=1.0
		self.writemasses()
		self.steps=24
		self.ALPHA=1.0
		self.omega=[0.0,0.0,0.0]
		self.SPMETHOD=1
		self.gamma=[]
		self.gammadata=''
		for i in range(self.NOB):
		  for j in range(0,self.steps+2):
		    self.gamma.append( map(lambda x: 0.0, range(self.dim) ) )  
		self.setgammadata()
		self.a=0
		self.h=0
		self.log=''
		self.todo=''


	def writemasses(self):
		self.massdata=""
		for i in range(self.NOB):
			self.massdata = self.massdata + " %.24e !! m(%i)\n" % (self.masses[i], i+1)
	  
	def new(self,SPMETHOD=1):
		"new path"
		self.SPMETHOD=SPMETHOD
		self.writemasses()
		self.setgammadata()
		self.confdata=_CONFIG_STRING % (
			"new",
			self.SPMETHOD,
			0,
			self.steps,
			self.ALPHA,
			self.omega[0],
			self.omega[1],
			self.omega[2]
			)
		alldata=self.confdata+  self.symdata + self.massdata + _endofchunk + "\n"
		if self.withcoll:
		  self.gammadata=_minorb_bin_withcoll(alldata) 
		else:
		  self.gammadata=_minorb_bin(alldata) 
		self.parsegammadata()
		return
	def load(self,mygfile):
		try:
			fd=open(mygfile,'r')
			self.gammadata=fd.read()
			fd.close()
		except:
			raise Exception("No file %s found" % mygfile )
		# set NOB,dim,STEPS ... see below
		self.parsegammadata()
	def write(self,mygfile):
		self.writemasses()
		self.setgammadata()
		fd=open(mygfile,'w')
		fd.write(self.gammadata)
		fd.close()
	# def relax(self,ALG, *args):
	def relax(*args):
		if len(args) == 1:
			print args[0].algs; return
		elif len(args) > 2:
			print "usage: relax(ALG)"; return
		self,ALG = args	
		self.confdata=_CONFIG_STRING % (
			"relax",
			3,
			ALG,
			self.steps,
			self.ALPHA,
			self.omega[0],
			self.omega[1],
			self.omega[2]
			)
		self.writemasses()
		self.setgammadata()
		alldata=self.confdata+ self.symdata + self.massdata +  _endofchunk+self.gammadata
		if self.withcoll:
		  self.gammadata=_minorb_bin_withcoll(alldata) 
		else:
		  self.gammadata=_minorb_bin(alldata) 
		# self.gammadata=_minorb_bin(alldata)
		self.parsegammadata()
		print " ==> action: %8.4f; howsol: %8.4e" % ( self.action(), self.howsol() )

	def newton(self,ALG):
		if (ALG / 100) == 0:
			print " Newton algorithm wrong ALG:", ALG
			return 
		self.confdata=_CONFIG_STRING % (
			"newton",
			3,
			ALG,
			self.steps,
			self.ALPHA,
			self.omega[0],
			self.omega[1],
			self.omega[2]
			)
		self.writemasses()	
		self.setgammadata()
		alldata=self.confdata+ self.symdata + self.massdata +  _endofchunk + self.gammadata
		if self.withcoll:
		  self.gammadata=_minorb_bin_withcoll(alldata) 
		else:
		  self.gammadata=_minorb_bin(alldata) 
		# self.gammadata=_minorb_bin(alldata)
		self.parsegammadata()
		print " ==> action: %8.4f; howsol: %8.4e" % ( self.action(), self.howsol() )
	def newtill(self,ALG,eps,maxiter=10):
	   for k in range(maxiter):
	     self.newton(ALG)
	     if self.h < eps:
		    return

	def printsol(self,nomefile):
		self.confdata=_CONFIG_STRING % (
			"printsol",
			0,
			0,
			self.steps,
			self.ALPHA,
			self.omega[0],
			self.omega[1],
			self.omega[2]
			)
		self.writemasses()	
		self.setgammadata()
		alldata=self.confdata+ self.symdata + self.massdata+ _endofchunk+ self.gammadata
		if self.withcoll:
		  printsol_data=_minorb_bin_withcoll(alldata) 
		else:
		  printsol_data=_minorb_bin(alldata) 
		# printsol_data=_minorb_bin(alldata)
		try:
			fd=open(nomefile,'w')
			fd.write(printsol_data)
			fd.close()
			self.viewfile=nomefile
		except:
			raise Exception("cannot write on file %s" % nomefile)
			self.viewfile=''
	def __repr__(self):
		if self.invalid:
			return "<minpath object; invalid>"
			del self	
		else:	
			return "<minpath object; NOB=%i, dim=%i, steps=%i>" % (self.NOB, self.dim, self.steps)
	def __str__(self):
		return repr(self)
		
	def dump(self):
		print ( self.symdata + self.massdata+ self.gammadata)
	def __len__(self):
		return self.steps
	def view(self,opts=''):
		"x.view(opts='--fstep=0 --withoff...')"
		self.printsol(_TMPFILE_NAME)
		## sys.stderr.write("Temporary file %s created...\n" % _TMPFILE_NAME)
		os.system(_ORBVIEW_PATH + (' %s ' % opts ) + _TMPFILE_NAME) ##TODO: change!
		os.remove(_TMPFILE_NAME)
		## sys.stderr.write("Temporary file %s removed...\n" % _TMPFILE_NAME)

	def reshape(self,steps):
		# write symfiledata...
		# left: NOB * (steps+2)
		zeros=[]
		newgamma=[]
		for i in range(self.dim):
			zeros.append(0.0)
		index=0
		array=range(self.NOB)
		for i in range(self.NOB):
			array[i]=range(self.steps+2)
			for j in range(self.steps+2):
				array[i][j]=self.gamma[index]
				index=index+1
		
		for i in range(self.NOB):
			newgamma.append( array[i][0] ) 
			for j in range(1,steps+1):  
				if j < self.steps+1:
					newgamma.append(array[i][j])
				else:	
					newgamma.append(zeros)
			newgamma.append(array[i][self.steps+1])
		self.steps=steps	
		self.gamma=newgamma
		self.setgammadata()
		return 

	def action(self):
		self.confdata=_CONFIG_STRING % (
			"action",
			3,
			0,
			self.steps,
			self.ALPHA,
			self.omega[0],
			self.omega[1],
			self.omega[2]
			)
		self.writemasses()	
		alldata=self.confdata+ self.symdata + self.massdata +  _endofchunk + self.gammadata
		if self.withcoll:
		  tmpvar=string.atof(_minorb_bin_withcoll(alldata))
		else:
		  tmpvar=string.atof(_minorb_bin(alldata))
		# tmpvar=string.atof(_minorb_bin(alldata))
		self.a=tmpvar
		return tmpvar
	def true_action(self):
	        return -1
	def howsol(self):
		self.confdata=_CONFIG_STRING % (
			"howsol",
			3,
			0,
			self.steps,
			self.ALPHA,
			self.omega[0],
			self.omega[1],
			self.omega[2]
			)
		self.writemasses()	
		alldata=self.confdata+ self.symdata + self.massdata +  _endofchunk+self.gammadata
		if self.withcoll:
		  tmpvar=string.atof(_minorb_bin_withcoll(alldata))
		else:
		  tmpvar=string.atof(_minorb_bin(alldata))
		# tmpvar = string.atof(_minorb_bin(alldata))
		self.h=tmpvar
		return tmpvar

	def info(self):
		str=self.symfilename
		## tmp=(string.split(str,os.sep))[-1]
		basename=string.join(string.split(str,'.')[0:-1],'.')
		sys.stdout.write("info on " + basename + ":\n")
		sys.stdout.write("----------------------------------------------------------------\n")
		tmpin = open(basename + '.info','r')
		info = tmpin.readlines()
		for li in info:
			if not re.match("^%",li):
				sys.stdout.write(li)
		tmpin.close()
		sys.stdout.write("----------------------------------------------------------------\n")
		print 

	def debug(self):
		self.writemasses()	
		alldata=self.confdata+ self.symdata + self.massdata + _endofchunk
		if self.withcoll:
		  tmpdata=_minorb_bin_withcoll(alldata)
		else:
		  tmpdata=_minorb_bin(alldata)
		# tmpdata=_minorb_bin(alldata) 
		print tmpdata
	def getsymfilename(self,dir):
		sys.stdout.write("\n")
		allfiles=[]
		symfiles=os.listdir(dir)
		for file_i in range(len(symfiles)):
			if os.path.splitext(symfiles[file_i])[1] == '.sym':
				allfiles = allfiles + [symfiles[file_i]]
		for i in range(len(allfiles)):
			sys.stdout.write(" %i)  %s\n" % (i+1,allfiles[i]))
		sys.stdout.write(" x)  eXit\n" )
		sys.stdout.write("\n  ...Select a Number: > ")
		line=sys.stdin.readline()[:-1]
		if line=="x":
			return ""
		try:
			answer=string.atoi(line)
			result=allfiles[answer-1]
			sys.stdout.write(" You have selected file: " + result + "\n")
			return result
		except:
			sys.stdout.write("wrong choice\n")
			self.getsymfilename(dir)
	def parsegammadata(self):
		self.gamma=[]
		self.masses=[]
		alllines=self.gammadata.split('\n')
		if len(alllines)<3:
		  sys.stderr.write("gammadata= " + self.gammadata)
		  raise Exception("no gammadata found...")
		else:
		  alllines=alllines[:-1]
		(self.NOB, self.dim, self.steps) = map(int, re.match(' *(\d+) +(\d+) +(\d+)',alllines[0] ).groups())
		headers = alllines[0:3]
		for i in range(len(headers)):
			headers[i] = re.compile(r"^ *").sub('',headers[i]) 
		tmp=string.split(headers[0],' ')[0:3]
		# self.NOB=string.atoi(tmp[0])	
		# self.dim=string.atoi(tmp[1])	
		# self.steps=string.atoi(tmp[2])
		tmp=string.split(headers[1],' ')[0:3]
		self.omega=[0.0,0.0,0.0]
		for i in range(3):
			self.omega[i] = string.atof(tmp[i])
		tmp=string.split(headers[2],' ')[0]
		self.ALPHA=string.atof(tmp)
		float_regex='[-+]?(?:\d+(?:\.\d*)?|\d*\.\d+)(?:[eE][-+]?\d+)?'
		numline_regex=' *' + ' +'.join(["(%s)" %float_regex]*self.dim)
		# self.omega = map(float, re.match(" *(%s) +(%s) +(%s)" % (float_regex,float_regex,float_regex),alllines[1] ).groups()) 
		#ignore masses ? ... written in gammadata...
		for line in alllines[3:3+self.NOB]:
		  self.masses.append(float( re.match(" *(%s)" % float_regex,line).groups()[0]) )
		self.writemasses()  
		for line in alllines[self.NOB+3:]:
		  numline=map(float, re.match(numline_regex,line).groups() )
		  self.gamma.append( numline )
		return 
	def setgammadata(self):
		self.gammadata=""" %i %i %i   !! NOB, dim, steps
 %.24e %.24e %.24e !! omega
 %.24e !! ALPHA
""" % (self.NOB, self.dim, self.steps, self.omega[0], self.omega[1], self.omega[2], self.ALPHA)
		self.writemasses()
		self.gammadata=self.gammadata + self.massdata
		for vect in self.gamma:
		  self.gammadata = self.gammadata + (" %.24e" * self.dim) %  tuple(vect) + "\n"
		return

	def copy(self):
		# write symfiledata...
		# other=minpath(self.symfilename)
		# other.gammadata=self.gammadata
		# other.parsegammadata()
		other=copy.deepcopy(self)
		return other
		 

	def __mul__(self,other):
		if type(other) == type(1.0) or type(other) == type(1):
		  result=self.copy()
		  for line in range(len(result.gamma)):
		    for i in range(self.dim):
		      result.gamma[line][i]=float(other) * self.gamma[line][i]
		  result.setgammadata()    
		else:
		  result=None
		return result


	def __rmul__(self,other):
		return self.__mul__(other)

	def __add__(self,other):
		if type(other) == type(self) and self.symfilename==other.symfilename and self.steps == other.steps and self.masses == other.masses and self.omega==other.omega:
		  result=self.copy()
		  for line in range(len(result.gamma)):
		    for i in range(self.dim):
		      result.gamma[line][i]=  self.gamma[line][i] + other.gamma[line][i]
		  result.setgammadata()    
		else:
		  result=None
		return result

	def __radd__(self,other):
		pass


def test_old():
	x=minpath("prova2.sym")
	x.new()
	x.relax(2)
	x.newton(2)
	x.write("prova.myg")
	# x.load("prova.myg")
	x.printsol("tmp.out")
	x.view()
	# print x


def scan_files(dir):
	symfiles=os.listdir(dir)
	for file_i in range(len(symfiles)):
		if os.path.splitext(symfiles[file_i])[1] == '.sym':
			print "    " + symfiles[file_i]
	print 		


############ pythoncard part
############ BEGIN
## from PythonCardPrototype import dialog, model
## from wxPython import wx
## import minimalDialog
############ pythoncard part
############ END




