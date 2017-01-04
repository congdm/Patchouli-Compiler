echo off
cd lib
echo on
..\bin\Poc /h Crypt.mod
..\bin\Poc /h Strings.mod
..\bin\Poc /h Rtl2.mod
..\bin\Poc /h Out.mod
echo off
copy /Y *.dll ..\test\
copy /Y *.sym ..\test\
move /Y *.dll ..\source\
move /Y *.sym ..\source\
cd ..
cd source
echo on
..\bin\Poc /h Scanner.mod
..\bin\Poc /h Base.mod
..\bin\Poc /h Generator.mod
..\bin\Poc /h Parser.mod
..\bin\Poc /h Poc.mod
echo off
cd ..
echo on

