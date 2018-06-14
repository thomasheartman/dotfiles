@echo off

DOSKEY code=start /b cmd /c "C:\Program Files\Microsoft VS Code Insiders\Code - Insiders.exe" $*

DOSKEY dot="C:\Program Files\Git\cmd\git.EXE" --git-dir=%HOME%\dotfiles\ --work-tree=%HOME% $*