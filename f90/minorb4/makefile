# minorb makefile
TODO=minorb.bin
########################################################################


FC=lf95
LIBS=-L/usr/local/imsl/lib/lf95 -lsmathd 
IMSLFLAGS=-I /usr/local/imsl/include/lf95
# FFLAGS= -O3
## FFLAGS= -g $(IMSLFLAGS)
# FFLAGS=  -g --ap --block 1024 --dal --dbl --f95 --in --info --long --o2 --tp4 --reace --trap --wo --verbose --warn --quad $(IMSLFLAGS)

# FFLAGS=  -g  --f95 --info --o2 --warn --block 1024 --ap --dal --unroll --trap --wo --varheap $(IMSLFLAGS)
FFLAGS= --f95  $(IMSLFLAGS)

# LDFLAGS=-static 
LDFLAGS= $(LIBS)

########################################################################

default:$(TODO)

test: 
	./minorb.bin < prova.init > tmp.out 

banale:  $(TODO)
	rm tmp.out
	./minorb.bin < banale.init > tmp.out 

otto:  $(TODO)
	rm tmp.out
	./minorb.bin < eight.init > tmp.out 
	
view:
	echo "plot 'tmp.out' u 1:2 w l, 'tmp.out' u 1:3 w l,\
	'tmp.out' u 1:4 w l, 'tmp.out' u 1:5 w l, \
	6*asin(1)*2*(x)**2 + sqrt(3)*asin(1)*2/(x),0, \
	12*asin(1)*2 * (x), - sqrt(3) * asin(1)*2 *(x)**(-2)" | gnuplot -persist 
########################################################################
.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) -c $(FFLAGS)  $< 

include .depend

########################################################################

clean:
	rm -f *.{o,mod}

########################################################################

#dependencies:

depend .depend: 
	@makedepf90 -o $(TODO) *.f90 > .depend

tar:
	(cd ..; tar zcvf /tmp/minorb4-`date +%F`.tgz  minorb4/minorb.bin minorb4/py/minpath minorb4/makefile)

backup:
	(cd ..; tar zcvf /tmp/minorb4-backup-`date +%F`.tgz  --exclude=examples minorb4)



