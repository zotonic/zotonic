@echo off
set erl=erl
set erlc=erlc
rem set erl="C:\Program Files\erl5.7.4\bin\erl.exe"
rem set erlc="C:\Program Files\erl5.7.4\bin\erlc.exe"

pushd .
cd .\deps\mochiweb\src
@echo on
@echo '------------------------------'
@echo 'make mochiweb'
@echo '------------------------------'
@echo off
set EBIN_DIR = "..\ebin"
if not exist "..\ebin" (
  mkdir ..\ebin
)
for %%g in (*.erl) do cmd /c "%erlc%" -o ../ebin/ %%g
copy *.app ..\ebin
popd


pushd .
cd .\deps\webzmachine
@echo on
@echo '------------------------------'
@echo 'make webzmachine'
@echo '------------------------------'
@echo off
%erl% -noinput +B -eval "case make:all() of up_to_date -> halt(0); error -> halt(1) end."
copy src\*.app ebin
popd

pushd .
cd .\deps\erlang-oauth
@echo on
@echo '------------------------------'
@echo 'make erlang-oauth'
@echo '------------------------------'
@echo off
IF NOT EXIST ebin (
	mkdir ebin
)
copy src\oauth.app ebin
%erl% -make

popd

pushd .
cd .\deps\gen_smtp\src
@echo on
@echo '------------------------------'
@echo 'make gen_smtp'
@echo '------------------------------'
@echo off
set EBIN_DIR = "..\ebin"
if not exist "..\ebin" (
  mkdir ..\ebin
)
for %%g in (*.erl) do cmd /c "%erlc%" -o ../ebin/ %%g
copy *.app ..\ebin
popd

@echo on
@echo '------------------------------'
@echo 'make zotonic'
@echo '------------------------------'
%erlc% -o src/erlydtl src/erlydtl/erlydtl_parser.yrl

%erl% -pa ebin ./erlang-oauth/ebin ./mochiweb/ebin ./webmachine/ebin -noinput +B -eval "case make:all() of up_to_date -> halt(0);error -> halt(1) end."

copy src\zotonic.app ebin

