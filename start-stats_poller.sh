#!/bin/sh
# -mnesia dir "'"$PWD"/Mnesia'"
cd `dirname $0`
exec erl -name ofs_store@127.0.0.1 -config $PWD/sys.config -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test -boot start_sasl -setcookie loom -s ofs_store_app