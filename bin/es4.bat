@echo off

SET BatFile=%~0
SET ThisDir=%BatFile:\es4.bat=%

echo %ThisDir%

PUSHD %ThisDir%
CALL %ThisDir%\es4.exe @MLton load-world %ThisDir%\es4.world -- %*
POPD
