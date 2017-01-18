TODO=otto

all: ppm mpeg_encode clean done

ppm:
	povray +Ipovray.ini  $(TODO).pov 

single:
	povray +Ipovray.ini  +KFI1 +KFF1 $(TODO).pov 


ffmpeg: 
	ffmpeg -r 25 -g 5 -intra -qscale 1 -hq -y -author "Davide L. Ferrario" -comment "Animazione di Orbite Periodiche" -s 800x600 -i "$(TODO)%03d.ppm" -f mpeg $(TODO).mpeg


mpeg_encode:
	@#mpeg_encode -realquiet  PARAMETER
	echo preferred ffmpeg

clean:
	rm *.ppm

done:
	echo "file output.mpeg created"
