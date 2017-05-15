echo off
cd test
..\build\Poc /B Buildfile /sym ..\source
cd ..
echo on
copy build\Oberon07.*.dll test\