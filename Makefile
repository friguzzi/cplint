SHELL := /bin/bash
#LIBDIR=lib/$(SWIARCH)/
SWIARCH=$(shell swipl --arch)
LIBDIR=lib/$(SWIARCH)/
#
#
CC=gcc
INC1=cudd-2.5.1/include
#INC2=$(shell while read one two three; \
#do TEMP=$two; \
#done <<< `whereis swipl`; \
#readlink -f $TEMP; \
)/include #da completare

INC2=${SWIHOME}/include
#INC2=`echo /usr/lib/swi*`/include/
INCDIRS= -I$(INC1) -I$(INC2)
CFLAGSBDDEM= $(CFLAGS) -fPIC -DBP_FREE -O3 -fomit-frame-pointer -Wall -g -O2 ${INCDIRS}

#
#
# You shouldn't need to change what follows.
#
LDFLAGS= $(LDSOFLAGS) -shared -Lcudd-2.5.1/cudd -Lcudd-2.5.1/mtr -Lcudd-2.5.1/st -Lcudd-2.5.1/util -Lcudd-2.5.1/epd -lcudd  -lmtr -lst -lepd -lutil -lm 

#


all:  bddem.so

bddem.so: bddem.o
	#swipl-ld -export-dynamic bddem.o  $(LDFLAGS) -o bddem.so  
	$(CC) -export-dynamic bddem.o  $(LDFLAGS) -o bddem.so  

#-Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

bddem.o : bddem.c
	cd cudd-2.5.1 && make && cd ..
	$(CC) -c $(CFLAGSBDDEM) bddem.c -o bddem.o

distclean: 
	rm -f *.o bddem.so
	cd cudd-2.5.1 && make distclean && cd ..
check:
	@echo "no check"
install: all
	cp bddem.so $(LIBDIR)
	
