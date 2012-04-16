@echo off
set erl=erl
set erlc=erlc
rem set erl="C:\Program Files\erl5.7.4\bin\erl.exe"
rem set erlc="C:\Program Files\erl5.7.4\bin\erlc.exe"

@echo ==============================
@echo  make deps
@echo ==============================

for /D %%i in (.\deps\*) do cmd /c .\deps\build-dep.cmd %%~ni %%i

cmd /c .\deps\build-dep.cmd erlang-oauth .\modules\mod_oauth\deps\erlang-oauth


@echo ==============================
@echo  make zotonic
@echo ------------------------------

setlocal enabledelayedexpansion
set DEPS=.\ebin .\modules\mod_oauth\deps\erlang-oauth\ebin
for /D %%i in (.\deps\*) do if exist %%i\ebin set DEPS=!DEPS! %%i

%erlc% -o src/erlydtl src/erlydtl/erlydtl_parser.yrl
%erl% -pa %DEPS% -noinput +B -eval "case make:all() of up_to_date -> halt(0);error -> halt(1) end."

copy src\zotonic.app ebin

@echo ------------------------------
@echo  build done
@echo ==============================
