echo off
cd test
..\build\Poc /B Buildfile /sym ..\source
cd ..
echo on
copy build\Patchouli.BigNums.dll test\
copy build\Oberon07.*.dll test\