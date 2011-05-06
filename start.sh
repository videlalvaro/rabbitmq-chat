#!/bin/sh
cd `dirname $0`
exec erl -sname rmqchat -pa $PWD/ebin $PWD/deps/*/ebin \
-boot start_sasl -config ./priv/chat -s rabbitmq_chat -detached