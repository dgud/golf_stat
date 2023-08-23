golf_stat
=====

A collection of some golf statistics tools

Build (on Windows)
-----

    $ rebar3 compile && ./make_release && (cd golfstat/; cmd.exe /C golf_stat.cmd [PersonalFile])

Run
-----

    $ cd golf_stats
    $ ./stats player_file.txt (will be created if it don't exist)

