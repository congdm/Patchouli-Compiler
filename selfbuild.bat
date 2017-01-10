echo off
cd source
..\build\Poc /B Buildfile /sym ..\lib;..\source
cd ..
echo on