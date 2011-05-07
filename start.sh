#!/bin/sh
cd `dirname $0`
exec erl -sname rmqchat -pa $PWD/ebin $PWD/deps/*/ebin \
-s rabbitmq_chat -config ./priv/chat -boot start_sasl -config ./priv/sasl -detached