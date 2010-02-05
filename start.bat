@echo off
set erl="C:\Program Files\erl5.7.4\bin\erl.exe"
set erlc="C:\Program Files\erl5.7.4\bin\erlc.exe"
set ebin=ebin deps\mochiweb\ebin  deps\erlang-oauth\ebin deps\webmachine\ebin

%erl% +P 10000000 -pa %ebin% -boot start_sasl -s zotonic
