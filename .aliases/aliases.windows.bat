@echo off

DOSKEY code=start /b cmd /c "code-insiders" $*

DOSKEY dot="C:\Program Files\Git\cmd\git.EXE" --git-dir=%USERPROFILE%\dotfiles\ --work-tree=%USERPROFILE% $*

DOSKEY pl="pijul" $*