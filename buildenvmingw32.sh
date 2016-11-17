# This file contains the environment that can be used to
# build the foreign pack outside Prolog.  This file must
# be loaded into a bourne-compatible shell using
#
#   $ source buildenv.sh


SWIPL='C:\Program Files (x86)\swipl\bin\swipl-win.exe'
SWIPLVERSION='70330'
SWIHOME='c:/program files (x86)/swipl'
SWIARCH='i386-win32'
PACKSODIR='lib/i386-win32'
SWISOLIB='-lswipl'
SWILIB='-lswipl'
CC='gcc'
LD='ld'
CFLAGS=' -I"c:/program files (x86)/swipl/include"'
LDSOFLAGS=' -shared -L"c:/program files (x86)/swipl/bin"'
SOEXT='dll'
TMP='C:\Users\fabrizio\AppData\Local\Temp'
TEMP='C:\Users\fabrizio\AppData\Local\Temp'

export  PATH SWIPL SWIPLVERSION SWIHOME SWIARCH PACKSODIR SWISOLIB SWILIB CC LD CFLAGS LDSOFLAGS SOEXT TMP TEMP
