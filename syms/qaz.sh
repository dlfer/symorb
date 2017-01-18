#!/bin/bash
cd n3d2
OUT=/tmp/tmpfile.txt
echo " PROCESSING FILES NOB=3 dim = 2" > $OUT
for file in *.info
do
basefile=$(basename $file .info)
cat <<__EOF__ >> $OUT


########################################################################
file: ${basefile}.sym
########################################################################
__EOF__
cat $file >> $OUT
done

a2ps -o /tmp/tmp.ps $OUT
rm $OUT
echo /tmp/tmp.ps generato
