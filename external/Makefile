.PHONY: all
all: compile-fftw3

fftw-3.3.10.tar.gz:
	wget http://fftw.org/fftw-3.3.10.tar.gz

fftw-3.3.10: fftw-3.3.10.tar.gz
	tar -xf $<
	cd fftw-3.3.10; ./configure --prefix=$(shell pwd)/../root

compile-fftw3: fftw-3.3.10
	cd fftw-3.3.10; make -j12; make -j12 install
