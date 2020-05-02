gfortran -cpp -c src/*.f90 fortran_utils/src/*.f90
gfortran *.o -o aoc2015

rm *.o

cp *.mod src/

time ./aoc2015 $1 $2