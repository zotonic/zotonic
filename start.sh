#!/bin/sh
cd `dirname $0`
exec erl +P 10000000 +K true -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s zotonic
