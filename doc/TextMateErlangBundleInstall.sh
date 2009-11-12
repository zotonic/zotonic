#!/bin/sh

LC_CTYPE=en_US.UTF-8
SVN=`which svn`

echo Changing to Bundles directory...
mkdir -p "/Library/Application Support/TextMate/Bundles"
cd "/Library/Application Support/TextMate/Bundles"

if [ -d "/Library/Application Support/TextMate/Bundles/Erlang.tmbundle" ]; then
	echo Erlang bundle already exists - updating...
	$SVN up "Erlang.tmbundle"
else
	echo Checking out Erlang bundle...
	$SVN --username anon --password anon co "http://svn.textmate.org/trunk/Bundles/Erlang.tmbundle"
fi

echo Reloading bundles in TextMate...
osascript -e 'tell app "TextMate" to reload bundles'
