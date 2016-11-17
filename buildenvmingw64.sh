# This file contains the environment that can be used to
# build the foreign pack outside Prolog.  This file must
# be loaded into a bourne-compatible shell using
#
#   $ source buildenv.sh

SWIPL='C:\Program Files\swipl\bin\swipl-win.exe'
SWIPLVERSION='70330'
SWIHOME='c:/program files/swipl'
SWIARCH='x64-win64'
PACKSODIR='lib/x64-win64'
SWISOLIB='-lswipl'
SWILIB='-lswipl'
CC='gcc'
LD='ld'
CFLAGS=' -I"c:/program files/swipl/include"'
LDSOFLAGS=' -shared -L"c:/program files/swipl/bin"'
SOEXT='dll'
TMP='C:\Users\fabrizio\AppData\Local\Temp'
TEMP='C:\Users\fabrizio\AppData\Local\Temp'

export  PATH SWIPL SWIPLVERSION SWIHOME SWIARCH PACKSODIR SWISOLIB SWILIB CC LD CFLAGS LDSOFLAGS SOEXT TMP TEMP
