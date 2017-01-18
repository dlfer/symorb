#!/usr/bin/env python
import os
import sys
import re
import string
import socket
import getopt

AllOptions = [ 
'help',
'ping',
'top',
'kill'
]
OptionsLetters=[]

def error_message():
  sys.stderr.write("Error\n")

def help_message():
  sys.stdout.write("no help yet\n")

try:
  options, xarguments = getopt.getopt(sys.argv[1:], OptionsLetters, AllOptions )
except getopt.error:
  error_message()
  sys.exit(1)


for a in options[:]:
  if ( a[0] == '--help'):
    help_message()
    sys.exit(0)

IS_KILL=0
for a in options[:]:
  if ( a[0] == '--kill'):
    IS_KILL=1

IS_TOP=0
for a in options[:]:
  if ( a[0] == '--top' ):
    IS_TOP=1
    options.remove(a)
    break

IS_PING=0
for a in options[:]:
  if ( a[0] == '--ping' ):
    IS_PING=1
    options.remove(a)
    break



_PING_COMMAND="ping  -q -n -c 1 %s 1>/dev/null"
_PACKETS="packets"
_LAMHOSTS_FILE=os.environ['HOME'] + "/local/etc/lamhosts.def"
_ALLHOSTS_FILE=os.environ['HOME'] + "/local/etc/allhosts.def"
_LAMBOOT_COMMAND="lamboot %s" % _LAMHOSTS_FILE
_SSH_CHECK_COMMAND="ssh -x %s echo \$TERM 1>/dev/null"
_SSH_MAJOR_PS_COMMAND="""ssh -x %s 'ps -e --format="%%C %%t  %%c (%%a) %%U"' """
_SSH_MINOR_PS_COMMAND="""ssh -x %s 'ps -e --format="%%C  %%c  %%U"' """
_SSH_PS_COMMAND='ssh -x %s ps -e --format="%%C"'
HOST_LIST=[]

def isUP(ip):
  sys.stderr.write("pinging ip %18s" % ip)
  result=os.system(_PING_COMMAND % ip )
  if result != 0:
    sys.stderr.write("   -->  DOWN!\n")
  else:
    sys.stderr.write("   -->  ok\n")
  return result==0

def strip_spaces(line):
        line=re.sub(" +"," ",line) 
        return re.sub("(\n)|(^ *)","",line)

def is_valid(line):
        if re.match("^#",re.sub("^ *","",line)) or line=="":
          return 0
        else:
          return 1

def update_hostlist():
    global HOST_LIST
    fd_in=open(_ALLHOSTS_FILE,'r')
    sys.stderr.write("reading data from file %s\n" % _ALLHOSTS_FILE)
    line='first'
    while line != '':
      line=fd_in.readline()
      line=strip_spaces(line)
      if is_valid(line) and isUP(line):
        HOST_LIST=HOST_LIST + [ line ]
    fd_in.close()


def lam_boot():
  global HOST_LIST
  sys.stderr.write("trying to boot the lam-mpid daemon...\n")
  os.system(_LAMBOOT_COMMAND) 

def pre_check():
  global HOST_LIST
  for h in HOST_LIST:
       sys.stderr.write("checking host %s... " % h )
       if not os.system(_SSH_CHECK_COMMAND % h) == 0:
	  raise "**error: something did not work at host %s**\n" % h
       else:
	  sys.stderr.write("ok\n")

def running_cpu(h):
     sys.stderr.write("connecting to %s to check PS...\n" % h)
     tmpfd=os.popen(_SSH_PS_COMMAND % h , 'r')
     result=tmpfd.readlines()
     tmpfd.close()
     total_cpu=0.0
     for l in result[1:]:
       total_cpu=total_cpu + string.atof(l)
     return total_cpu 

def major_running_cpu(h):
     sys.stderr.write("connecting to %s to check PS...\n" % h)
     tmpfd=os.popen(_SSH_MAJOR_PS_COMMAND % h , 'r')
     result=tmpfd.readlines()
     tmpfd.close()
     major_procs=''
     for l in result[1:]:
       tmpvar=string.split(strip_spaces(l),' ')[0]
       if string.atof(tmpvar)> 3:
         major_procs=major_procs+l+"\n"
     return major_procs

def minor_running_cpu(h):
     sys.stderr.write("connecting to %s to check PS...\n" % h)
     tmpfd=os.popen(_SSH_MINOR_PS_COMMAND % h , 'r')
     result=tmpfd.readlines()
     tmpfd.close()
     major_procs=''
     for l in result[1:]:
       tmpvar=string.split(strip_spaces(l),' ')[0]
       if string.atof(tmpvar)> 0:
         major_procs=major_procs+l+"\n"
     return major_procs


def hostlist_write():
    global HOST_LIST
    numHosts=0
    fd_out=open(_LAMHOSTS_FILE,'w')
    for line in HOST_LIST:
        totcpu=running_cpu(line)
        if totcpu < 110.0 or line==HOST_LIST[0]:
    	  fd_out.write("%s\n"  % (line) )
          sys.stderr.write("adding host %16s    (%%CPU=%04.2f)\n" % (line,totcpu) )
	  numHosts += 1
        else:
          sys.stderr.write("**skipped host %16s (%%CPU=%04.2f)\n" % (line,totcpu)) 
	  major_procs=major_running_cpu(line)
	  sys.stderr.write("**major procs:\n%s" % major_procs )

    fd_out.close()
    sys.stdout.write("...done! %i hosts (1 master + %i slaves)\n" % (numHosts, numHosts-1) )


def pre_ps():
  global HOST_LIST
  for h in HOST_LIST:
     total_cpu=running_cpu(h)
     print "host %16s: %04.2f" % (h,total_cpu)
       
def sub_top():
    user_dic={}
    for line in HOST_LIST[1:]:
        sys.stdout.write(major_running_cpu(line))
        stringa=string.split(minor_running_cpu(line),"\n")
	for item in stringa:
	   tokens=string.split(strip_spaces(item), ' ' )
	   if len(tokens)>1:
	     try:
	       username=tokens[2]
	       cpuperc=string.atof(tokens[0])
	       command=tokens[1]
	       if username in user_dic.keys():
	         if command in (user_dic[username]).keys():
		   user_dic[username][command] = user_dic[username][command] + cpuperc / float(len(HOST_LIST))
		 else:
		   user_dic[username][command] = cpuperc / float(len(HOST_LIST))
	       else:
	         user_dic[username]={}
		 user_dic[username][command] = cpuperc / float( len(HOST_LIST) )
	     except:
	       sys.stderr.write("line %s corrupted...\n" % item )
    sys.stdout.write("\n\n=========================================================\n")  
    sys.stdout.write("=== result on %i hosts: =================================\n" % len(HOST_LIST) ) 
    for username in user_dic.keys():
      sys.stdout.write("=========================================================\n")  
      tot_cpu=0
      for command in (user_dic[username]).keys():
        tot_cpu=tot_cpu + user_dic[username][command]
      sys.stdout.write("%9s: %4.2f%%\n" % (username,tot_cpu)  )
      for command in (user_dic[username]).keys():
        sys.stdout.write("         \-->  %12s: %4.2f%%\n" % (command,user_dic[username][command] ) )
    return 	



if __name__=="__main__":
  if IS_TOP:
    update_hostlist()
    sub_top()
  elif IS_PING:
    update_hostlist()
  else:  
    update_hostlist()
    pre_check()
    hostlist_write()
    lam_boot()
  sys.exit(0)  
