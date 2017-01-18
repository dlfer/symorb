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
	// #write (MyFile, "array: ", Count1, "\n")
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
