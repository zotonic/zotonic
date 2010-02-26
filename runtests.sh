#!/bin/sh
#
# Copyright 2010 Arjan Scherpenisse
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
cd `dirname $0`

# Find all tests
MODULES=`ls ebin/*_tests.beam|sed 's/.beam//'|sed 's/ebin\///'`

# Skip postgres tests
MODULES=`echo $MODULES|sed 's/pgsql_pool_tests//'|sed 's/pgsql_tests//'`

# Run the tests
ALL="zotonic"
for MODULE in $MODULES; do ALL="$ALL,$MODULE"; done
#exec erl -noshell -pa ebin -eval "eunit:test([$ALL],[verbose]),init:stop()"

echo $ALL
exec erl erl +P 10000000 +K true -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -sasl errlog_type error -s zotonic -eval "eunit:test([$ALL],[verbose]),init:stop()"

