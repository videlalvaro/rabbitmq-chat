#!/bin/sh
cd `dirname $0`
exec erl -sname rmqchat_admin -pa $PWD/../ebin #\
#-s rmqchat_utils stop $1 $2 -noshell -s init stop