SHELL := /bin/bash
#LIBDIR=lib/$(SWIARCH)/
LIBDIR=lib/$(SWIARCH)/
#
#
CC=gcc
INC1=cudd-3.0.0/cudd
CURRDIR=$(shell pwd)
#INC2=$(shell while read one two three; \
#do TEMP=$two; \
#done <<< `whereis swipl`; \
#readlink -f $TEMP; \
)/include #da completare

INC2=${SWIHOME}/include
#INC2=`echo /usr/lib/swi*`/include/
INCDIRS= -I$(INC1)
CFLAGSBDDEM= $(CFLAGS) -fPIC -DBP_FREE -O3 -fomit-frame-pointer -Wall -g -O2 ${INCDIRS}

#
#
# You shouldn't need to change what follows.
#
LDFLAGS= $(LDSOFLAGS) -shared -Lcudd-3.0.0/cudd/.libs/ -lcudd
#cudd-3.0.0/cudd/.libs/libcudd-3.0.0.so.0.0.0

#


all:  bddem.$(SOEXT)

bddem.$(SOEXT): bddem.o
	#swipl-ld -export-dynamic bddem.o  $(LDFLAGS) -o bddem.$(SOEXT)
	if test $(SWIARCH) == x64-win64 -o $(SWIARCH) == i386-win32 ; then \
  $(CC) bddem.o -static-libgcc -static-libstdc++  $(LDFLAGS) -lswipl -o bddem.$(SOEXT) ;\
   else  \
     if [[ $(SWIARCH) ==  *darwin* ]] ;  then \
  $(CC) bddem.o  $(LDFLAGS) -lswipl -o bddem.$(SOEXT) ;\
  else  \
  $(CC) -export-dynamic bddem.o  $(LDFLAGS) -o bddem.$(SOEXT) ;\
  fi \
  fi
#-Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

bddem.o : bddem.c
	cd cudd-3.0.0 &&  make && cd ..
	$(CC) -c $(CFLAGSBDDEM) bddem.c -o bddem.o


distclean: clean
	rm Makefile
	cd cudd-3.0.0 && make distclean && cd ..

clean:
	rm -f *.o bddem.$(SOEXT)

check:
	@echo "no check"

install: all
	mkdir -p $(LIBDIR)
	cp bddem.$(SOEXT) $(LIBDIR)

installcheck:
	swipl -g test -t halt prolog/cplint_test/test.pl
