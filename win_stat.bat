@echo off

IF "%~1"=="" (
   echo Error: need a filename
   goto continue
)

call "c:\Program Files\erl-24.0\bin\erl.exe" -pa _build/default/lib/golf_stat/ebin -run gs_gui start_halt %~1

:continue

