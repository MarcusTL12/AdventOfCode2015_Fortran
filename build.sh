gfortran -cpp -c src/*.f90
gfortran *.o -o aoc2015

rm *.o

time ./aoc2015 $1 $2