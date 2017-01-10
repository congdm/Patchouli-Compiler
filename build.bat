echo off
cd build
..\bin\Poc /B Buildfile /sym ..\lib;..\source
cd ..
echo on