FC = gfortran
CC = gcc
RC = R CMD SHLIB
FFLAGS = -O4 -shared -fPIC -xf77-cpp-input
CFLAGS = -O4 -shared -fPIC -Df2cFortran -I/usr/lib/R/include \
         -I/usr/local/lib/R/include -I/usr/local/include 

all: algencan.so

algencan.so: rwrapper.o algencan.o
	$(RC) rwrapper.o algencan.o -o algencan.so

rwrapper.o: rwrapper.c rwrapper.h cfortran.h
	$(CC) $(CFLAGS) -c rwrapper.c

algencan.o: algencan.f
	$(FC) $(FFLAGS) -c algencan.f
