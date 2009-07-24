@echo off
c:\lazarus\fpc\2.2.2\bin\x86_64-win64\gorc.exe /machine x64 /nw /ni /r /fo E:\ksp\KSP2009\kspplayer\dlls\version\ksp.res ksp.rc
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
