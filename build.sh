gfortran -cpp -c fortran_utils/src/*.f90 src/datastructures/*.f90 src/*.f90
gfortran *.o -o aoc2015

rm *.o

cp *.mod src/
cp *.mod src/datastructures/

time ./aoc2015 $1 $2