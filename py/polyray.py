#!/usr/bin/env python2.7

import pickle
from Numeric import *
import shutil
import tempfile
import time
import os
import getopt
import traceback
import sys
from math import *
import string
import re

PARI_EXECUTABLE = "gp -q"
PARI_MAX_DEGREE = 7

MPEG_COMMAND = 'ffmpeg'

POVRAY_EXECUTABLE = "povray +L/usr/lib/povray3/include "
POVRAY_EXECUTABLE = "povray +L/usr/lib/povray3/include -GD -GS -GR -GW +GF"

POVRAY_EXECUTABLE_LOCAL = "povray -GD -GS -GR -GW +GF"
POVRAY_EXECUTABLE_LOCAL = "/usr/bin/povray +L/usr/lib/povray3/include -GD -GS -GR -GW +GF"

POVRAY_EXECUTABLE_REMOTE = "pvmpov +L/home/ferrario/local/src/pvmpov3_1g_2/povray31/include -GD -GS -GR -GW +GF"
POVRAY_EXECUTABLE = POVRAY_EXECUTABLE_LOCAL


# -------------------------------------------------------------------------#
def vector_product(u, v):
    return (u[1]*v[2] - u[2] * v[1], u[2]*v[0] - u[0]*v[2], u[0]*v[1] - u[1] * v[0])

# -------------------------------------------------------------------------#


def replace_exp(str):
    return str.replace('^', '**')

# -------------------------------------------------------------------------#


class ClassLogError:
    def __init__(self):
        self.stderr = sys.stderr
        self.stdout = sys.stdout
        self.hostname = os.uname()[1]

    def msg(self, msg):
        self.stdout.write(("[-pypov (%s)-]: " % self.hostname) + msg + "\n")
        self.stdout.flush()

    def error(self, msg):
        self.stderr.write(("[*error (%s)*]: " % self.hostname) + msg + "\n")
        self.stderr.flush()


global LOG
LOG = ClassLogError()

# -------------------------------------------------------------------------#


class TempDir:
    def __init__(self):
        self.NAME = tempfile.mktemp('_tmp')
        if not os.path.exists(self.NAME):
            LOG.msg("creating temporary dir %s..." % self.NAME)
            os.mkdir(self.NAME)
        else:
            LOG.error(
                "Fatal Error: cannot create temp dir; fix $TMPDIR env variable!")
            sys.exit(2)

    def __del__(self):
        LOG.msg("removing recursively temporary dir %s..." % self.NAME)
        shutil.rmtree(self.NAME)

        # -------------------------------------------------------------------------#


def get_opt():
    AllOptions = ['html', 'remote=']
    OptionsLetters = ''

    def help_message():
        sys.stdout.write("no help\n")

    def error_message():
        sys.stderr.write("no error message\n")

    try:
        options, xarguments = getopt.getopt(sys.argv[1:],
                                            OptionsLetters, AllOptions)
    except getopt.error:
        error_message()
        sys.exit(1)

    for a in options[:]:
        if (a[0] == '--html'):
            IS_HTML = 1
            options.remove(a)
            break

    IS_REMOTE = 0
    for a in options[:]:
        if (a[0] == '--remote'):
            IS_REMOTE = 1
            REMOTE_FILE = a[1]
            POVRAY_EXECUTABLE = POVRAY_EXECUTABLE_REMOTE
            options.remove(a)
            break

    if len(xarguments) > 0:
        poly = string.join(xarguments)
    else:
        poly = ''

# print "poly= [", poly , "]"


# =========================================================================#
# pi=3.141592654 (in math)
radfactor = .01745329252
# =========================================================================#

povray_command_options = {
    'Library_Path': 'L',
    'Height': 'H',
    'Width': 'W',
    'Pause_when_Done': 'P',
    'Bounding_Threshold': 'MB',
    'Test_Abort': 'X',
    'Test_Abort_Count': 'X',
    'DISPLAY': 'D',
    'Quality': 'Q',
    'Initial_Frame': 'KFI',
    'Final_Frame': 'KFF',
    'Initial_Clock': 'KI',
    'Final_Clock': 'KF',
    'Cyclic_Animation': 'KC',
    'Output_File_Type': 'F',  # (C)ompressed TGA, P(N)G, (P)PM, and  (T)GA.
    'Output_File_Name': 'O',
    'Verbose': 'V',
    'Antialias': 'A',
    'Jitter': 'J',
    'Antialias_Depth': 'R',
    'Light_Buffer': 'UL',
    'Vista_Buffer': 'UV'
}

povray_default_options = {
    'Pause_when_Done': 'off',
    'Bounding_Threshold': 25,
    'Test_Abort': 'on',
    'Test_Abort_Count': 10,
    'DISPLAY': 'off',
    'Quality': '9',
    'Output_File_Type': 'ppm',  # (C)ompressed TGA, P(N)G, (P)PM, and  (T)GA.
    'Verbose': 'off',
    'Antialias': 'on',
    'Light_Buffer': 'on',
    'Vista_Buffer': 'on'
}

Video_Options = {
    'fps': 25,
    'codec': 'mpeg4'
}

Video_Extensions = (
    '.mpeg', '.mpg', '.avi'
)

##
# =============================================================


def make_into_string(db):
    result = ' +Iinput.pov'
    for kk in povray_default_options:
        if not kk in db and kk != 'Output_File_Type':
            db[kk] = povray_default_options[kk]
    for k in db:
        if k == 'Output_File_Name':
            base, ext = os.path.splitext(db[k])
            if ext.lower() == '.tga':
                result += " +FC"
            elif ext.lower() == '.png':
                result += " +FN16"
            elif ext.lower() == '.ppm':
                result += ' +FP'
            else:
                result += ' +FP'
            if ext.lower() in Video_Extensions:
                result += " +%s%s" % (
                    povray_command_options[k], "%s.ppm" % base)
            else:
                result += " +%s%s" % (povray_command_options[k], db[k])
        elif str(db[k]).lower() == 'on':
            result += "  +%s" % povray_command_options[k]
        elif str(db[k]).lower() == 'off':
            result += "  -%s" % povray_command_options[k]
        elif k.lower() in Video_Options:
            pass
        else:
            result += " +%s%s" % (povray_command_options[k], db[k])
    return result


# =============================================================
class PovRay:
    def __init__(self, **args):
        self.povray_options_db = args
        self.cwdir = os.getcwd()
        k = 'Output_File_Name'
        base, ext = os.path.splitext(args[k])
        if ext.lower() in Video_Extensions:
            LOG.msg("video processing selected")
            self.do_video = True
        else:
            self.do_video = False
        # now compute Initial_Clock
        # and Final_Clock
        if not 'Initial_Clock' in args:
            self.Initial_Clock = 0.0
            if self.do_video:
                self.povray_options_db['Initial_Clock'] = self.Initial_Clock
        else:
            self.Initial_Clock = args['Initial_Clock']
        if not 'Final_Clock' in args:
            self.Final_Clock = 1.0
            if self.do_video:
                self.povray_options_db['Final_Clock'] = self.Final_Clock
        else:
            self.Final_Clock = args['Final_Clock']
        if 'fps' in args:
            self.fps = args['fps']
        else:
            self.fps = 25
        if not 'Initial_Frame' in args:
            self.Initial_Frame = 0
            if self.do_video:
                self.povray_options_db['Initial_Frame'] = self.Initial_Frame
        self.number_of_frames = int(
            self.fps * (self.Final_Clock - self.Initial_Clock))
        if not 'Final_Frame' in args:
            self.Final_Frame = self.number_of_frames
            if self.do_video:
                self.povray_options_db['Final_Frame'] = self.Final_Frame
        self.number_of_digits = len(str(self.number_of_frames))
        self.input_ffmpeg_file = base + '%' + \
            ("0%i" % self.number_of_digits)+'d.ppm'
        self.output_file_name = args['Output_File_Name']
        self.mpeg_command = MPEG_COMMAND + ' -r %i -qmin 1 -qmax 2 -g 25 -hq -y -author "Davide L. Ferrario" -comment "Animazione" -s %ix%i -i "%s" -f avi %s' % (
            self.fps, args['Height'], args['Width'], self.input_ffmpeg_file, self.output_file_name)

    def esegui(self, str):
        return self.src_esegui(str)

    def src_esegui(self, str):
        start_time = time.time()
        TMPDIR = TempDir()
        fd = open(os.path.join(TMPDIR.NAME, 'input.pov'), 'w')
        fd.write(str)
        fd.close()
        os.chdir(TMPDIR.NAME)
        povray_options = make_into_string(self.povray_options_db)
        LOG.msg("executing command: " + POVRAY_EXECUTABLE +
                " " + povray_options + "...")
        if os.system(POVRAY_EXECUTABLE + " " + povray_options):
            LOG.error("***POVRAY ERROR***")
        if self.do_video:
            LOG.msg("\n ora si esegue il comando: " +
                    self.mpeg_command + "...")
            if os.system(self.mpeg_command):
                LOG.error("***FFMPEG ERROR***")
        end_time = time.time()
        if 'Output_File_Name' in self.povray_options_db:
            nomefile = self.povray_options_db['Output_File_Name']
            shutil.copy(os.path.join(TMPDIR.NAME, nomefile), self.cwdir)
            delta_time = end_time-start_time
            ore = int(delta_time / (60.0*60.0))
            minuti = int((delta_time - 3600.0 * ore) / 60.0)
            if minuti == 1:
                plurale = 'o'
            else:
                plurale = 'i'
            if ore == 1:
                ore_msg = "1 ora, "
            elif ore > 1:
                ore_msg = "%i ore, " % ore
            else:
                ore_msg = ""
            secondi = delta_time - 60 * minuti
            LOG.msg("File %s generato in %s%i minut%s e %.2f secondi" %
                    (nomefile, ore_msg, minuti, plurale, secondi))
        else:
            nomefile = ''
            LOG.error("Filename not found...")
        os.chdir(self.cwdir)
        del TMPDIR
        return (end_time-start_time)

    def __del__(self):
        pass  # self.pipe_out.close()

    def set_host(self, remote_host):
        self.remote_host = remote_host


# =============================================================
class RemPovRay(PovRay):
    def esegui(self, str):
        self.todo_world = str
        rem = Remote(self.remote_host)
        rem.rexec(self, 'run()')
        # and sync the output file...
        rem.ssh.get(self.output_file_name, self.output_file_name)

    def run(self):
        self.src_esegui(self.todo_world)


##
# =============================================================
class Pari:
    def __init__(self):
        self.pipe_in, self.pipe_out = os.popen2(PARI_EXECUTABLE)

    def esegui(self, str):
        self.pipe_in.write(str)
        self.pipe_in.close()
        return self.pipe_out.read()

    def __del__(self):
        self.pipe_out.close()


class FakePari:
    def __init__(self):
        pass

    def esegui(self, str):
        return """2
1,
0,
0,
0,
1,
0,
1,
0,
1,
-1,
"""

    def __del__(self):
        pass

###
# =============================================================
# data stolen from polyray


class WimsData:
    def __init__(self):
        self.f_dull = 'specular 0.2 ambient 0.15 roughness 0.3 diffuse 0.4'
        self.f_shiny = 'specular 1 roughness 0.001 diffuse 0.55 reflection 0.05'
        self.f_glossy = 'specular 1 roughness 0.0006 reflection 0.15'
        self.f_luminous = 'ambient 1  diffuse 0'
        self.f_metal = 'ambient 0.25 brilliance 4 diffuse 0.5 metallic specular 0.80 roughness 1/80 reflection 0.5'
        self.f_mirror = 'ambient 0.02  diffuse 0 reflection 1 roughness 0.01'
        self.c_transparent = 'color rgbf<0.71, 0.65, 0.26, 0.8>'
        self.f_glass = 'F_Glass9'


##
Predefined = WimsData()

# =============================================================


class PovObject:
    def __init__(self):
        self.fakedata = ''
        self.r_angle = '0'
        pass

    def rotate(self, r_angle):
        self.r_angle = " ( %s  +  %s  )" % (self.r_angle, r_angle)

    def povray(self):
        return "// -- begin object: " + " -- //\n" + self.data + "// -- end object: " + " -- //\n"

    def finish(self, arg):
        try:
            return getattr(Predefined, "f_"+arg)
        except:
            LOG.error("argument %s does not exist:" % arg)
            return getattr(Predefined, "f_dull")

# =============================================================


class PovData(PovObject):
    def __init__(self, data):
        self.data = data

###
# =============================================================


class Polynomial(PovObject):
    def __init__(self, str, r_angle='0', pigment="Brass", finish="metal", center=(0, 0, 0), scale=1):
        self.source = str
        self.data = ''
        self.scale = scale
        self.num_coeffs = []
        self.coeffs = ''
        self.degree = -1
        self.r_angle = r_angle
        self.pigment = pigment
        self.real_finish = self.finish(finish)
        self.center = center
        pari = Pari()
        all_data = pari.esegui("""p=%s;
deg=poldegree(subst(subst(subst(p,x,a*X),y,b*X),z,c*X),X)
if(deg==1,deg=2);
if(deg>%i || deg<2,quit);
for(i=0,deg,for(j=0,i,for(k=0,j,print(polcoeff(polcoeff(polcoeff(p,deg-i,x),i-j,y),j-k,z),","))));
""" % (str, PARI_MAX_DEGREE))
        all_data = string.split(all_data, "\n")
        self.degree = string.atoi(all_data[0])
        for line in all_data[1:-1]:
            self.num_coeffs += [line[:-1]]
        self.coeffs = string.join(self.num_coeffs, ',')
        self.make_povray(zone=2)

    def make_povray(self, zone=1):
        scale = 1.0/zone*self.scale
        transform = "translate <(%f)*(%f),(%f)*(%f),(%f)*(%f)> rotate <0,0,(%s)>" % (
            self.center[0], scale, self.center[1], scale, self.center[2], scale, self.r_angle)
        output = """poly { %i < %s > sturm
       scale <%f,%f,%f>
       clipped_by { // box {<-1,-1,-1> <1,1,1>} 
       sphere { <0,0,0>, 2 }
       } 
       bounded_by {clipped_by}
       texture { 
         pigment { %s } 
         finish { %s }  
	 }
       texture { TextureAxes scale 0.2 }
        %s 
       } """ % (self.degree, self.coeffs, scale, scale, scale, self.pigment, self.real_finish, transform)
        self.data = output
        # return output

    def __repr__(self):
        return self.source

# =============================================================


class PovWorld:
    def __init__(self, background='SkyBlue'):
        self.data = """
#include "colors.inc"
#include "textures.inc"
#include "finish.inc"
#include "metals.inc"
#include "glass.inc"
#include "chars.inc"

// global_settings { 
// radiosity {
// distance_maximum 0.2
// nearest_count 10
// count 1000
// gray_threshold 0.3
// }}

global_settings { ambient_light  %s 
// radiosity {
// distance_maximum 0.2
// nearest_count 10
// count 1000
// gray_threshold 0.3
// }
}

background {color %s }

#declare TextureAxes = texture {
pigment{ gradient x scale 1.0
                color_map{[0.000   color rgbt<0,0,0,0.4>]
                          [0+0.02 color rgbt<0,0,0,0.4>]
                          [0+0.02 color rgbt <1,1,1,1>]
                          [1-0.02 color rgbt <1,1,1,1>]
                          [1-0.02 color rgbt<0,0,0,0.4>]
                          [1.000   color rgbt<0,0,0,0.4>]} }
finish { ambient 0.15 diffuse 0.85}
}
texture {
pigment{ gradient x scale 1.0 rotate 90*z
                color_map{[0.000   color rgbt<0,0,0,0.4>]
                          [0+0.02 color rgbt<0,0,0,0.4>]
                          [0+0.02 color rgbt <1,1,1,1>]
                          [1-0.02 color rgbt <1,1,1,1>]
                          [1-0.02 color rgbt<0,0,0,0.4>]
                          [1.000   color rgbt<0,0,0,0.4>]} }
finish { ambient 0.15 diffuse 0.85}
}

// #declare TextureAxesUV = texture {
// uv_mapping pigment{ gradient x scale 1.0
// color_map{[0.000   color rgbt<0,0,0,0.4>]
// [0+0.02 color rgbt<0,0,0,0.4>]
// [0+0.02 color rgbt <1,1,1,0.3>]
// [1-0.02 color rgbt <1,1,1,0.3>]
// [1-0.02 color rgbt<0,0,0,0.4>]
// [1.000   color rgbt<0,0,0,0.4>]} }
// uv_mapping pigment{ gradient x scale 1.0 rotate 90*z
// color_map{[0.000   color rgbt<0,0,0,0.4>]
// [0+0.02 color rgbt<0,0,0,0.4>]
// [0+0.02 color rgbt <1,1,1,0.3>]
// [1-0.02 color rgbt <1,1,1,0.3>]
// [1-0.02 color rgbt<0,0,0,0.4>]
// [1.000   color rgbt<0,0,0,0.4>]}
// }
// finish { ambient 0.15 diffuse 0.85}
// }
""" % (background, background)

    def write(self, filename):
        fd = open(filename, 'w')
        fd.write("// -- generato automaticamente... -- //\n")
        fd.write(self.data)
        fd.close()

    def append(self, data):
        self.data += data.povray()


# objects


# =============================================================
class light_source(PovObject):
    def __init__(self, position=(0, 0, -10), colour="1"):
        self.position = position
        self.colour = colour
        self.make_povray()

    def make_povray(self):
        self.data = """
light_source { <%f, %f, %f >  color %s }
""" % (self.position[0], self.position[1], self.position[2], self.colour)

# =============================================================


class camera(PovObject):
    def __init__(self, scale=1, v_angle=25, h_angle=10):
        self.v_angle = v_angle * radfactor
        self.h_angle = h_angle
        self.scale = scale
        self.make_povray()

    def make_povray(self):
        self.data = """camera {
location %f*<0,-15*cos( %s ),15*sin( %s )> 
look_at 0 angle 16 right <1,0,0> up <0,sin(%s ),cos(%s)>
rotate <0,0,%s>
}
light_source  { %f * < 0,-15*cos( %s ),15*sin( %s ) > 
color 0.7*White  transmit 0.4
// color rgbt <0.0,0,1.0,.9> 
rotate <0,0,%s>
}
""" % (self.scale, self.v_angle, self.v_angle, self.v_angle, self.v_angle, self.h_angle, self.scale, self.v_angle, self.v_angle, self.h_angle)


class Sphere(PovObject):
    def __init__(self, position="<0,0,0>", radius="1", finish="metal", pigment="Brass"):
        self.position = position
        self.radius = radius
        self.texture = """texture {
pigment { %s  }
finish { %s }
}
""" % (pigment, self.finish(finish))
        self.make_povray()

    def make_povray(self):
        self.data = """sphere {
%s, %s
%s
}
""" % (self.position, self.radius, self.texture)

# =============================================================


class axes(PovObject):
    def __init__(self, r_angle='0'):
        self.data = ''
        self.r_angle = r_angle
        self.make_povray()

    def make_povray(self):
        self.data = """
union {
cylinder {<-1.4,0,0> <1.4,0,0> .015}
cylinder {<0,-1.4,0> <0,1.4,0> .015}
cylinder {<0,0,-1.4> <0,0,1.4> .015}
cone {<1.45,0,0> 0 <1.35,0,0> 0.045}
cone {<0,1.45,0> 0 <0,1.35,0> 0.045}
cone {<0,0,1.45> 0 <0,0,1.35> 0.045}
polygon {5 <-1.4,0.08-0.02,0.12>
<-1.4,0.08+0.02,0.12>
<-1.4,-0.08+0.02,-0.12>
<-1.4,-0.08-0.02,-0.12>
<-1.4,0.08-0.02,0.12> }
polygon {5 <-1.4,-0.08-0.02,0.12>
<-1.4,-0.08+0.02,0.12>
<-1.4,0.08+0.02,-0.12>
<-1.4,0.08-0.02,-0.12>
<-1.4,-0.08-0.02,0.12> }
polygon {10 <0.08+0.02,-1.4,0.12>
<0.08-0.02,-1.4,0.12>
<0,-1.4,0.02*0.5>
<-0.08+0.02,-1.4,0.12>
<-0.08-0.02,-1.4,0.12>
<-0.02*0.9,-1.4,-0.02*1.5>
<-0.02*0.9,-1.4,-0.12>
<0.02*0.9,-1.4,-0.12>
<0.02*0.9,-1.4,-0.02*1.5>
<0.08+0.02,-1.4,0.12> }
pigment {color <1,1,1>} 
finish {specular 0.2 ambient 0.15 roughness 0.3 diffuse 0.4} 
translate
<-(0)*(1/(2)),-(0)*(1/(2)),-(0)*(1/(2))>
rotate <0,0,-(%s)> 
}
""" % self.r_angle


class Plane(PovObject):
    def __init__(self):
        self.data = ''
        self.make_povray()

    def make_povray(self):
        self.data = """
plane {z, -1 
scale <0.5,0.5,1>
clipped_by {box {<-2,-2,-0.99> <2,3,-1.01>}} bounded_by {clipped_by}
texture  { 
pigment { Grey }
finish {specular 0.2 ambient 0.15 roughness 0.3 diffuse 0.4}
}
texture { TextureAxes scale 0.5 }
}
"""


# =============================================================

class Parametric(PovObject):
    def __init__(self, eqns="(s,t,0)", smin=-pi, smax=pi, tmin=-pi, tmax=pi, ssteps=24, tsteps=24, pigment="Brass", finish="metal", wireframe=True, **args):
        # self.eqns=eqns[1:-1].split(",")
        # self.eqns=(eqns.split("(")[1]).split(")")[0].split(',')
        self.eqns = replace_exp(eqns)
        self.pigment = pigment
        texture = """pigment { %s  }
    finish { %s } """ % (pigment, self.finish(finish))
        if 'texture' in args:  # overwrite...
            texture = args['texture']
        if type(texture) == type(()) and len(texture) == 2:
            self.textures = texture
        else:
            self.textures = (texture,) * 2
        if 'pythoncode' in args:
            self.pythoncode = args['pythoncode']
            del args['pythoncode']
        else:
            self.pythoncode = None
        self.args = args
        self.grid = [[]]
        self.normals = [[]]
        self.uv_values = [[]]
        self.data = ''
        self.start_s = smin
        self.end_s = smax
        self.start_t = tmin
        self.end_t = tmax
        self.Ns = ssteps
        self.Nt = tsteps
        self.delta_s = (smax - smin) / float(ssteps)
        self.delta_t = (tmax - tmin) / float(tsteps)
        # def todofunc(s,t): return tuple([eval(self.eqns[ii_c]) for ii_c in [0,1,2]])

        def todofunc(s, t):
            db = locals()
            for k in self.args:
                db[k] = self.args[k]
            for k in globals():
                db[k] = globals()[k]
            if self.pythoncode:
                exec((self.pythoncode), db)
            db['s'] = s
            db['t'] = t
            return eval(self.eqns, globals(), db)
        # def todofunc(s,t): return eval(self.eqns)
        self.todofunc = todofunc

        self.make_grid()
        self.make_normals()
        self.make_povray()
        if wireframe:
            self.make_wireframe()

    def todofunc_normal(self, s, t):
        # return the normal at the point.
        s_p = s + 0.1 * self.delta_s
        s_m = s - 0.1 * self.delta_s
        t_p = t + 0.1 * self.delta_t
        t_m = t - 0.1 * self.delta_t
        tangent_s = (array(self.todofunc(s_p, t)) -
                     array(self.todofunc(s_m, t))) / self.delta_s * 10.0
        tangent_t = (array(self.todofunc(s, t_p)) -
                     array(self.todofunc(s, t_m))) / self.delta_t * 10.0 / 1000.0
        return vector_product(tangent_s, tangent_t)

    def evalfunc(self, iis, iit):
        return self.todofunc(self.start_s + iis * self.delta_s, self.start_t + iit * self.delta_t)

    def evalfunc_normals(self, iis, iit):
        result = self.todofunc_normal(
            self.start_s + iis * self.delta_s, self.start_t + iit * self.delta_t)
        result_norm = math.sqrt(sum(array(result)**2))
        if result_norm > 0.001:
            return tuple(array(result) / result_norm)
        else:
            vv = array(self.todofunc_normal(self.start_s + (iis-1)
                       * self.delta_s, self.start_t+iit * self.delta_t))
            vv += array(self.todofunc_normal(self.start_s + (iis+1)
                        * self.delta_s, self.start_t+iit * self.delta_t))
            vv += array(self.todofunc_normal(self.start_s + (iis)
                        * self.delta_s, self.start_t+(iit-1) * self.delta_t))
            vv += array(self.todofunc_normal(self.start_s + (iis) *
                        self.delta_s, self.start_t+(iit+1) * self.delta_t)) * 0.25
            result_norm = math.sqrt(sum(array(vv)**2))
            if result_norm == 0:
                result_norm = 1
            return tuple(vv / result_norm)

    def make_grid(self):
        self.grid = [[self.evalfunc(Is, It) for It in range(self.Nt)]
                     for Is in range(self.Ns)]
        self.uv_values = [[(self.start_s + Is*self.delta_s, self.start_t+It*self.delta_t)
                           for It in range(self.Nt)] for Is in range(self.Ns)]

    def make_normals(self):
        self.normals = [[self.evalfunc_normals(
            Is, It) for It in range(self.Nt)] for Is in range(self.Ns)]

    def make_povray(self):
        self.textures_mesh = (
            'texture {TextureMeshA}', 'texture {TextureMeshB}')
        output = """
#declare TextureMeshA = texture { %s }
#declare TextureMeshB = texture { %s }
mesh {
""" % self.textures
        for Is in range(self.Ns):
            for It in range(self.Nt):
                output += """smooth_triangle 
 { <%f,%f,%f>, <%f,%f,%f>, <%f,%f,%f> , <%f,%f,%f>, <%f,%f,%f>, <%f,%f,%f> 
 uv_vectors <%f, %f>, <%f,%f>, <%f,%f> 
   %s  
 }
      """ % (
                    self.grid[Is][It] + self.normals[Is][It] +
                    self.grid[Is][(It+1) % self.Nt] + self.normals[Is][(It+1) % self.Nt] +
                    self.grid[(Is+1) % self.Ns][It] + self.normals[(Is+1) % self.Ns][It] +
                    self.uv_values[Is][It] + self.uv_values[Is][(It+1) % self.Nt] + self.uv_values[(Is+1) % self.Ns][It] +
                    (self.textures_mesh[(It + Is) % 2],)
                )
                output += """smooth_triangle { 
  <%f,%f,%f>, <%f,%f,%f>, <%f,%f,%f>, <%f,%f,%f>, <%f,%f,%f>, <%f,%f,%f> 
  uv_vectors <%f, %f>, <%f,%f>, <%f,%f> 
   %s  
}
""" % (
                    self.grid[Is][(It+1) % self.Nt] + self.normals[Is][(It+1) % self.Nt] +
                    self.grid[(Is+1) % self.Ns][(It+1) % self.Nt] + self.normals[(Is+1) % self.Ns][(It+1) % self.Nt] +
                    self.grid[(Is+1) % self.Ns][It] + self.normals[(Is+1) % self.Ns][It] +
                    self.uv_values[Is][(It+1) % self.Nt] + self.uv_values[(Is+1) % self.Ns][(It+1) % self.Nt] + self.uv_values[(Is+1) % self.Ns][It] +
                    (self.textures_mesh[(It + Is) % 2],)
                )
        output += """
}"""
        self.data = output

    def make_wireframe(self):
        aa = array(self.grid)
        for Is in range(self.Ns):
            self.data += Tube(aa[Is, :, :], radius=0.01,
                              pigment="Bronze2", finish="metal").data
        for It in range(self.Nt):
            self.data += Tube(aa[:, It, :], radius=0.01,
                              pigment="Bronze2", finish="metal").data

# =============================================================


class PovParametric(PovObject):
    # Functions (+,-,*,/, abs, sqr, sqrt, cub, x^y, exp, log, sin, cos, atan, sinh, cosh, tanh, asinh, min, max)
    """It does not work..."""

    def __init__(self, eqns="(u,v,0)", umin=-pi, umax=pi, vmin=-pi, vmax=pi, texture=("pigment {color Black}", "pigment {color White}"), pigment="Brass", finish="metal", **args):
        reg = re.compile('\(([^,]+),([^,]+),([^,]+)\)')
        # self.eqns=(eqns.split("(")[1]).split(")")[0].split(',')
        try:
            self.eqns = reg.search(eqns).groups()
        except:
            raise Exception("fallito eqns")
        if type(texture) == type(()) and len(texture) == 2:
            self.textures = texture
        else:
            self.textures = (texture,) * 2
        self.args = args
        self.pigment = pigment
        self.texture = """pigment { %s  }
    finish { %s } """ % (pigment, self.finish(finish))

        self.umin = umin
        self.umax = umax
        self.vmin = vmin
        self.vmax = vmax
        self.make_povray()

    def make_povray(self):
        self.data = """
parametric { 
function { %s },
function { %s },
function { %s }
< %f , %f > < %f , %f >
contained_by { box {<-10,-10,-10> <10,10,10>}  }
texture{ %s  }
}""" % (self.eqns[0], self.eqns[1], self.eqns[2],  self.umin, self.umax,
            self.vmin, self.vmax, self.texture)

# =============================================================


class Tube(PovObject):
    def __init__(self, points_array, radius=0.02, pigment="Green", finish="dull", type='linear', **args):
        self.texture = """pigment { %s  }
finish { %s } """ % (pigment, self.finish(finish))
        self.points_array = array(points_array)
        self.radius = radius
        self.data = ''
        self.tolerance = ''
        if type == 'linear':
            self.type = 'linear_spline'
            self.tolerance = 'tolerance 0.0001'
        elif type == 'cubic':
            self.type = 'cubic_spline'
            initial_point = 2 * \
                self.points_array[0, :] - self.points_array[1, :]
            final_point = 2*self.points_array[-1, :]-self.points_array[-2, :]
            self.points_array = concatenate(
                (array([initial_point]), self.points_array, array([final_point])))
            self.tolerance = 'tolerance 0.01'
        self.make_povray()

    def make_povray(self):
        points_data = '\n'.join(
            [("<%s,%s,%s>, %s" % (tuple(x) + (self.radius,))) for x in self.points_array])
        # print [(tuple(x) + (self.radius,)) for x in self.points_array  ]
        self.data = """
sphere_sweep {
%s %i,
%s
%s
texture{ %s  }
}""" % (self.type, len(self.points_array), points_data, self.tolerance, self.texture)


# =============================================================

class Curve(PovObject):
    def __init__(self, eqns="(t,t,0)", tmin=-pi, tmax=pi, tsteps=24, pigment="OrangeRed", finish="glossy", radius=0.02, **args):
        self.eqns = replace_exp(eqns)
        self.pigment = pigment
        self.finish = finish
        self.args = args
        self.Nt = tsteps
        self.data = ''
        self.start_t = tmin
        self.radius = radius
        self.delta_t = (tmax - tmin) / float(tsteps)

        def todofunc(t):
            db = self.args
            self.args['t'] = t
            return eval(self.eqns, globals(), self.args)
        self.todofunc = todofunc
        self.make_grid()
        self.make_povray()

    def evalfunc(self, iit):
        return self.todofunc(self.start_t + iit * self.delta_t)

    def make_grid(self):
        self.grid = [self.evalfunc(It) for It in range(self.Nt+1)]

    def make_povray(self):
        self.data = Tube(array(self.grid), radius=self.radius,
                         pigment=self.pigment, finish=self.finish, type='cubic').data


# =============================================================


class RemoteSH:
    def __init__(self, host):
        self.host = host

    def system(self, command):
        return os.system("""ssh -x  %s "%s" """ % (self.host,  command))

    def put(self, file_here, file_there):
        return os.system("""scp %s %s:%s""" % (file_here, self.host, file_there))

    def get(self, file_there, file_here):
        return os.system("""scp %s:%s %s""" % (self.host, file_there, file_here))

# =============================================================


class SyncTempObj:
    def __init__(self, host):
        self.NAME = tempfile.mktemp("_tmp")
        self.ssh = RemoteSH(host)
        self.host = host

    def __del__(self):
        LOG.msg("removing  %s..." % self.NAME)
        os.remove(self.NAME)
        LOG.msg("removing  %s:%s..." % (self.host, self.NAME))
        return self.ssh.system("rm %s" % self.NAME)

    def dump(self, obj):
        fd = open(self.NAME, 'w')
        pickle.dump(obj, fd)
        fd.close()
        return self.ssh.put(self.NAME, self.NAME)

    def write(self, str):
        fd = open(self.NAME, 'w')
        fd.write(str)
        fd.close()
        return self.ssh.put(self.NAME, self.NAME)

    def update(self):
        return self.ssh.get(self.NAME, self.NAME)

# =============================================================


class Remote:
    def __init__(self, host):
        self.host = host
        self.ssh = RemoteSH(host)
        self.tempobj = SyncTempObj(host)
        self.temppy = SyncTempObj(host)

    def rexec(self, object, todo):
        if IS_REMOTE:
            return self._remote_rexec()
        else:
            object.TODOLIST = string.split(todo, ';')
            return self._local_rexec(object)

    def _local_rexec(self, object):
        # send an object, load object, execute object in environment...
        self.tempobj.dump(object)
        self.temppy.write(open(sys.argv[0], 'r').read())
        self.ssh.system("python %s --remote=%s %s" %
                        (self.temppy.NAME, self.tempobj.NAME, self.tempobj.NAME))

    def _remote_rexec(self):
        strobj = REMOTE_FILE
        print("opening the object...")
        temp_this_object = pickle.load(open(strobj))
        print("done")
        for command in temp_this_object.TODOLIST:
            sys.stderr.write("executing command: " + command)
            if command != "":
                eval("temp_this_object." + command)


# =============================================================
# class of a n-body animation...
class MinPath:
    def __init__(self, filename):
        fd = open(filename, 'r')
        original_line = '#'
        nowhere_land = 1
        dim = 0
        body = 0
        time = 0
        steps = 0
        path = []
        data = []
        maxima = [0, 0, 0]
        comments = []
        while original_line != '':
            original_line = fd.readline()
            if string.count(original_line, '#') > 0:
                comments.append(original_line)
                continue
            line = string.replace(original_line, "\n", '')
            fields = string.split(line)

            # chunck of blank lines...
            if len(fields) == 0:
                nowhere_land = 1
                if (time > 0) and (steps == 0):
                    steps = time
                elif (time != steps):
                    LOG.error("fatal error! steps not homogeneous!")
                    LOG.error("check the data file!!!")
                    sys.exit(1)
                continue
            elif (dim == 0):
                dim = len(fields)
            elif (dim != len(fields)):
                LOG.error("fatal error: dimension error!")
                sys.exit(1)

            if (len(fields) > 0):
                if nowhere_land == 1:
                    nowhere_land = 0
                    if (body > 0):
                        data.append(path)
                    path = []
                    body = body + 1
                    time = 0
                time = time + 1
            for i in range(0, len(fields), 1):
                fields[i] = string.atof(fields[i])
                maxima[i] = max(abs(fields[i]), maxima[i])
            if (dim == 2):
                fields.append(0)
            path.append(fields)
        if (body > 0):
            data.append(path)
        self.NOB = len(data)
        self.steps = steps
        self.dim = dim
        self.data = data
        fd.close()

    def make_povinc(self):
        data = self.data
        all_steps = len(data[0])-1
        povinc_string = """// path generated by %s  version %1.2f
// %s 

#declare NOB=%i;
#declare ALLSTEPS=%i;

#declare MyArray = array[NOB][ALLSTEPS+1];

// --BEGIN DATA--
""" % (sys.argv[0], 0.0, os.uname(), self.NOB, all_steps)

        for i in range(self.NOB):
            povinc_string = povinc_string + "\n// BODY number %i\n" % (i + 1)
            for j in range(len(data[0])):
                povinc_string = povinc_string + \
                    "#declare MyArray[%i][%i] = < %f , %f , %f >;\n" % (
                        i, j, self.data[i][j][0], self.data[i][j][1], self.data[i][j][2])
        povinc_string = povinc_string + "\n// --END DATA--\n"
        return povinc_string


# --------------------------------------------------------------------
class PovMinPath(PovObject):
    def __init__(self, filename, show=[], hide=[]):
        mp = MinPath(filename)
        self.NOB = mp.NOB
        self.show = show
        self.hide = hide
        self.data = mp.make_povinc()
        self.data += Orbview_Inc
        self.data += """

#declare PathRadius  = 0.1;
#declare Sphere_Size = 0.16934500031693656;
#declare Cylinder_Radius = Sphere_Size / 6;
#declare Cylinder_Tip = 0.1; // -0.3
#declare Number_Of_Loops = 6;

Make_PATHS(Cylinder_Tip,Cylinder_Radius)

"""
        for i in range(self.NOB):
            if i in self.show or (not i in self.hide):
                self.data += "PATHS[%i]\n" % i

        self.data += "Make_SPHERES ( clock / 6.0  )\n"


# --------------------------------------------------------------------

Orbview_Inc = """
// 

#declare BallColor = array[NOB];
#declare PathColor = array[NOB];


// default colors

#declare Index=0;
#while (Index < NOB)

#switch (Index)
#case ( 0 )
	#declare BallColor[Index] = OrangeRed;
#break
#case ( 1 )
	#declare BallColor[Index] = SummerSky;
#break
#case ( 2 )
	#declare BallColor[Index] = Yellow;
#break
#case ( 3 )
	#declare BallColor[Index] = Green;
#break
#case ( 4 )
	#declare BallColor[Index] = Cyan;
#break
#case ( 5 )
	#declare BallColor[Index] = Magenta;
#break
#case ( 6 )
	#declare BallColor[Index] = Brown;
#break
#else
#declare Ballcolor[Index] = rgb < rand(Index), rand(Index+1), rand(Index+2) >;
#end


#declare PathColor[Index] = BallColor[Index];  
#declare Index = Index +1;
#end


#declare TRAJ = array[NOB];
#declare Count1 = 0;
#while (Count1 < NOB)
	#declare TRAJ[Count1] = spline { linear_spline
		#declare Count2 = 0;
		#while (Count2 < ALLSTEPS+1)
			Count2 / ALLSTEPS, MyArray[Count1][Count2]
		#declare Count2 = Count2 + 1;
		#end 

	}
	#declare Count1 = Count1 + 1;
#end




#macro Make_PATHS (cylinder_tip,cylinder_radius)
	#declare PATHS = array[NOB];
	#declare Count1 = 0;
	#while (Count1 < NOB)
		#declare PATHS[Count1] = union {
			#declare Count2 = 0;
			#while (Count2 < ALLSTEPS)
				cylinder{
				(1+cylinder_tip)*MyArray[Count1][Count2] -cylinder_tip* MyArray[Count1][Count2+1]  ,
				(1+cylinder_tip)*MyArray[Count1][Count2+1] -cylinder_tip* MyArray[Count1][Count2]  ,
				cylinder_radius open}
			#declare Count2 = Count2 + 1;
			#end // Count2
			pigment {color PathColor[Count1] }
		};
	#declare Count1 = Count1 + 1;
	#end // Count1

#end // macro


#macro Make_SPHERES (ttime)
	#declare time = (ttime - floor(ttime));
	#declare Count=0;
	#while( Count < NOB)
		sphere { TRAJ[Count](time),
		Sphere_Size
		 texture{
		    pigment { color  BallColor[Count] }
		    finish { ambient 0.4 diffuse 0.6 phong 1 }
		     }
	 	  }
	#declare Count = Count +1 ;
	#end // while

#end // macro
"""

# ------------------------------------------------------------------------


if __name__ == '__main__':
    get_opt()
    PW = PovWorld(background="SkyBlue")
    PW.append(camera(scale=1.2, h_angle=90, v_angle=90))
    PW.append(light_source(position=(-12, -18, 15), colour="<1,1,1>"))
    PW.append(light_source(position=(12, -18, 15), colour="<1,1,0>"))
    PW.append(Plane())
    # xp=PovRay(Width=600,Height=600,Output_File_Name='prova_eight.mpeg',Initial_Clock=0.0,Final_Clock=6.0,Cyclic_Animation='On')
    xp = PovRay(Width=600, Height=600, Output_File_Name='prova_eight.ppm')
    xp.set_host('localhost')
    PW.append(PovMinPath('prova.data', hide=[1, 2]))

    # print(PW.data)
    xp.esegui(PW.data)

    # rem=Remote('giobbe1.matapp.unimib.it')
    # rem.rexec(xp,'run()')
