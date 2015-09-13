# TABLE O CONTENTS #

REQUIREMENTS
INSTALLATION (default)
INSTALLATION (MySQL)
LOGS
INSTALLATION FROM SOURCES
IMPORTANT

# REQUIREMENTS #

Windows XP/Vista/7
BASS audio library (http://un4seen.com)
Sqlite3.dll from http://sqlite.org
QT 4.5 (http://qtsoftware.com)

All required libraries are also in installation pack

# INSTALLATION (default) #

By default KSP uses Sqlite3 database. That database doesn't require extra configuration which means that KSP will configure database automatically.

# LOGS #

To make debugging process easier KSP uses logging system. All logs are in folder: <user profile>\.KSP\logs\<log date> where <user profile> is the name of user folder (usually C:\Users\<user name>) and <log date> is folder containing KSP startup date and time in it's name. Last folder in <user profile>\.KSP\logs\<user profile>\.KSP\logs\ contains the most recent logs.

Logs are created per thread to make it easier for developers to establish which thread is causing troubles and when. To help us with debugging process always remember to attach all recent logs to your bug report.

# INSTALLATION FROM SOURCES #

KSP requires QT 4.5.x. You can obtain it either from http://qtsoftware.com or (better  way) from KSP installation. If you have already installed KSP 2009R2 pre or newer then you should have all required libraries in KSP folder (only Windows, only 32 bit libraries).

In order to compile KSP from sources you have to install Lazarus IDE from http://www.lazarus.freepascal.org/. Remember that KSP compilation requires Lazarus IDE 0.9.29 or newer. After that follow the steps:

1. Download QT bindings version 1.72 from http://users.telenet.be/Jan.Van.hijfte/qtforfpc/fpcqt4.html and open it
2. Extract qt4.pas to c:\lazarus\lcl\interfaces\qt (replace file when prompted). Of course assuming you have installes Lazarus in c:\lazarus
3. Run Lazarus and choose Tools->Configure Build Lazarus->Advanced Build Options
4. As LCL interface choose QT and for LCL part choose Build or Clean and Build
5. Click Build button. If everything goes fine after a few minutes you should have QT interface support recompiled.

Now Lazarus is ready to compile KSP. In order to get KSP from sourcs you need either to download sources from KSP website or from SVN. Remember that SVN version is not thought to be officially released. It might contain fixes for old bugs found in last release as well as contain new ones. Use it with caution. To learn how to get sources from SVN go to http://code.google.com/p/kspnew/source/checkout.

1. Create folder ksp (you can call it whatever you like)
2. In folder ksp create folders bin, dcu, po and src
3. Put KSP sources in ksp\src folder
4. Open ksp\src\ksp.lpr and build it
5. If compilation was successful in ksp\bin there should appear ksp.exe file  and in ksp\po there should be ksp.po file
6. You can safely remove ksp\bin\ksp.dbg file (it's debugger information)
7. From original KSP instalation copy all missing dll libraries (bass**.dll, lib**.dll, sqlite3.dll, Qt