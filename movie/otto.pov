// povray try file.
#version 3.2;
#include "colors.inc"
#include "textures.inc"
#include "glass.inc"


// light sources

light_source { < -254.017500 , 0.000000 , 0.000000 > colour 1 }
light_source { < 0.000000 , 0.000000 , 254.017500 > colour 1 }
light_source { < 0.000000 , 254.017500 , 0.000000 > colour 1 }

// splines and other data
#include "tmp2.inc" // define MyArray + NOB + allsteps

#include "orbview.inc" // define TRAJ

#declare PathRadius  = 0.1;
#declare Sphere_Size = 0.16934500031693656;
#declare Cylinder_Radius = Sphere_Size / 6;
#declare Cylinder_Tip = -0.3;
#declare Number_Of_Loops = 6

Make_PATHS(Cylinder_Tip,Cylinder_Radius)



PATHS[0]
// PATHS[1]
// PATHS[2]

// #include "extra.inc"

// now moving objects (time =0..1)

// camera (later: rotating)
camera {
location  < -6.0803500095080967 , 2*sin( pi * clock/36.0) , 0 >
look_at   < 0 , 0 , 0 >
rotate <0, -(clock ) *10,0> 
}



Make_SPHERES ( clock / 12.0  )


