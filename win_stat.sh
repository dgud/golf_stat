#!/bin/sh

if [ x"$1" = x"" ]
then
  echo Error: need a filename
  exit
fi

File=`wslpath -m "$1"`

"/mnt/c/Program Files/erl-24.0/bin/erl.exe" -pa _build/default/lib/golf_stat/ebin -run gs_gui start_halt "$File"
