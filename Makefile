LIBDIR=lib/$(SWIARCH)
#
#
CC=gcc
CFLAGSBDDEM= $(CFLAGS) -shared -fPIC -DBP_FREE -O3 -fomit-frame-pointer -Wall -g -O2 -Icudd-2.5.0/include

#
#
# You shouldn't need to change what follows.
#
INSTALL=/usr/bin/install -c
LDFLAGS= $(LDSOFLAGS) -Lcudd-2.5.0/cudd -Lcudd-2.5.0/mtr -Lcudd-2.5.0/st -Lcudd-2.5.0/util -Lcudd-2.5.0/epd -lcudd  -lmtr -lst -lepd -lutil -lm 

#


all:  bddem.so

bddem.so: bddem.o
	$(CC) -export-dynamic  $(LDFLAGS) bddem.o -o bddem.so  

#-Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

bddem.o : bddem.c
	cd cudd-2.5.0 && make && cd ..
	$(CC) -c $(CFLAGSBDDEM) bddem.c -o bddem.o

distclean: 
	rm -f *.o bddem.so
check:
install: default
	$(INSTALL_PROGRAM) bddem.so $(LIBDIR)
