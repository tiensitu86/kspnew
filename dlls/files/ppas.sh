#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling kspfiles
/usr/bin/as --64 -o kspfiles.o kspfiles.s
if [ $? != 0 ]; then DoExitAsm kspfiles; fi
echo Linking libkspfiles.so
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --build-id -z noexecstack -init FPC_LIB_START -fini FPC_LIB_EXIT -soname libkspfiles.so -shared -L. -o libkspfiles.so link.res
if [ $? != 0 ]; then DoExitLink libkspfiles.so; fi
IFS=$OFS
echo Linking libkspfiles.so
OFS=$IFS
IFS="
"
/usr/bin/strip --discard-all --strip-debug libkspfiles.so
if [ $? != 0 ]; then DoExitLink libkspfiles.so; fi
IFS=$OFS
