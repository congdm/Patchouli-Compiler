#!/bin/sh

set +x
cd source
wine ../build/Poc.exe /B Buildfile
cd ..
set -x
