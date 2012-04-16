@echo off

set escript="escript.exe"

@echo ------------------------------
@echo  make %1
@echo ------------------------------

pushd .
cd %2

set EBIN_DIR=.\ebin
if not exist %EBIN_DIR% (
  mkdir %EBIN_DIR%
)

if exist .\rebar (
  %escript% .\rebar get-deps
  %escript% .\rebar compile
) else if exist .\Emakefile (
  %erl% -make
) else (
  for %%g in (.\src\*.erl) do cmd /c "%erlc%" -o %EBIN_DIR% %%g
)

if exist .\src\*.app (
  copy .\src\*.app %EBIN_DIR%
)

popd
@echo Leaving Directory: %2
