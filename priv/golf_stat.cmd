@echo off
setlocal
set script=%~f0

for %%F in ("%script%") do set dirname=%%~dpF

rem "%dirname%\bin\erl.exe" -run gs_gui start_halt %*
"%dirname%\bin\erl.exe" -detached -run gs_gui start_halt %*

