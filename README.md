# symorb


```
to install:
# check where the gap folder
export GAPPATH=/usr/lib/gap
./configure
make
```

Read `f90/minorb4/README` to compile **minorb.bin**

Documentation in `./doc`

To install the package in a directory DIR not in the root path,
it is necessary to run gap with the '-l' switch.
E.g. add alias gap='gap -l "GAP_DIR;DIR"' to .profile


Set SYMORBDIR, as the directory where symorb is installed (e.g.
```
~/local/symorb
```
) and then execute `${SYMORBDIR}/bin/minpath`


