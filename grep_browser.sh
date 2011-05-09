#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -boot start_sasl -config ./priv/sasl -s browser browse -noshell -s init stop