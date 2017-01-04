echo off
cd ..
cd lib
echo on
..\build\Poc Crypt.mod
..\build\Poc Strings.mod
..\build\Poc Rtl.mod
..\build\Poc Out.mod
echo off
copy /Y *.dll ..\test\
copy /Y *.sym ..\test\
move /Y *.dll ..\source\
move /Y *.sym ..\source\
cd ..
cd source
echo on
..\build\Poc Scanner.mod
..\build\Poc Base.mod
..\build\Poc Generator.mod
..\build\Poc Parser.mod
..\build\Poc Poc.mod
echo off
cd ..\build
echo on

