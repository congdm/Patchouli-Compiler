set +x
cd test
wine ../build/Poc /B Buildfile /sym ../source
cd ..
set -x
cp build/Patchouli.BigNums.dll test/
cp build/Oberon07.*.dll test/
