#!/bin/sh
cd `dirname $0`
exec erl -sname rmqchat -pa $PWD/ebin $PWD/deps/*/ebin \
-boot start_sasl -sasl_dev ./priv/chat -s rabbitmq_chat
