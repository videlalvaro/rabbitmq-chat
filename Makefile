DEPS_DIR=deps
EBIN_DIR=ebin

all: compile

clean:
	rm -rf $(EBIN_DIR)

$(DEPS_DIR):
	mkdir $(DEPS_DIR);
	wget http://www.rabbitmq.com/releases/plugins/v2.4.1/amqp_client-2.4.1.ez
	wget http://www.rabbitmq.com/releases/plugins/v2.4.1/rabbit_common-2.4.1.ez
	unzip -d deps amqp_client-2.4.1.ez
	unzip -d deps rabbit_common-2.4.1.ez
	./rebar get-deps
	rm amqp_client-2.4.1.ez
	rm rabbit_common-2.4.1.ez

compile: $(DEPS_DIR)
	./rebar compile