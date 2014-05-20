#!/bin/sh
# -mnesia dir "'"$PWD"/Mnesia'"
cd `dirname $0`
exec erl -sname ofs_store -config $PWD/sys.config -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test -boot start_sasl -cookie ofs_store -s ofs_store_app