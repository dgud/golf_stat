@echo off

IF "%~1"=="" (
   echo Error: need a filename
   goto continue
)

call "c:\Program Files\Erlang OTP\bin\erl.exe" -pa _build/default/lib/golf_stat/ebin -run gs_gui start_halt %~1

:continue

