#!/bin/sh

set +x
cd build
wine ../bin/Poc.exe /B Buildfile /sym "..\\source"
cd ..
set -x
