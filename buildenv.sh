# This file contains the environment that can be used to
# build the foreign pack outside Prolog.  This file must
# be loaded into a bourne-compatible shell using
#
#   $ source buildenv.sh

PATH='/usr/lib/swi-prolog/bin/amd64:/home/nick/bin:/home/nick/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/nick/.shbin:/snap/bin:/usr/lib/R:/usr/lib/jvm/java-8-oracle/bin:/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin'
SWIPL='/usr/lib/swi-prolog/bin/amd64/swipl'
SWIPLVERSION='70603'
SWIHOME='/usr/lib/swi-prolog'
SWIARCH='amd64'
PACKSODIR='lib/amd64'
SWISOLIB=''
SWILIB='-lswipl'
CC='gcc'
LD='gcc'
CFLAGS='-pthread -fPIC -Wdate-time -D_FORTIFY_SOURCE=2 -fPIC -D_GNU_SOURCE -I/usr/include/ncursesw -I"/usr/lib/swi-prolog/include"'
LDSOFLAGS='-rdynamic -Wl,-Bsymbolic-functions -Wl,-z,relro -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -pthread   -shared'
SOEXT='so'
USER='nick'
HOME='/home/nick'

export  PATH SWIPL SWIPLVERSION SWIHOME SWIARCH PACKSODIR SWISOLIB SWILIB CC LD CFLAGS LDSOFLAGS SOEXT USER HOME
