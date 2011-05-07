# RabbitMQ Chat #

This project shows how to create a simple web chat room with RabbitMQ.

The chat server uses Websockets to send messages to the connected users. This means it only works with Chrome and Safari. Some browsers like Firefox 4 can be configured to enable Websockets.

On the RabbitMQ side there it uses a _custom exchange_ to store the last 20 messages, so new users connecting to the chat room can get a context of what's going on. See the project [Recent History Exchange](https://github.com/videlalvaro/rabbitmq-recent-history-exchange) for installation instructions.

This will be used as an example for the book [RabbitMQ in Action](http://bit.ly/rabbitmq).

## Requirements ##

The [Recent History Exchange](https://github.com/videlalvaro/rabbitmq-recent-history-exchange) _has to be installed_ prior to running this application.

## How does it work ##

Each user that connects to the server will get a _private auto delete anonymous queue_. This queue will be managed by an AMQP consumer. Whenever the AMQP consumer recieves a message it will forward it to the Websockets process and this one will send it to the user browser.

When the user is disconnects from the server his consumer gets closed and the queue deleted.

## Building: ##

Get the source code:

    $ git clone git://github.com/videlalvaro/rabbitmq-chat.git
    $ cd rabbitmq-chat

Compile the source code:

    $ make

## Usage ##

- Install the [Recent History Exchange](https://github.com/videlalvaro/rabbitmq-recent-history-exchange) plugin if you haven't done so.

- Start RabbitMQ if is not running already.

- Start the server:


    $ ./start-dev.sh

Point your browser to [http://localhost:8080/](http://localhost:8080/). Invite your friends. Enjoy!

## Credits ##

Inspired by [YakRiak](https://github.com/seancribbs/yakriak)

Original design adapted from __YakRiak__. Thanks [Basho](http://basho.com/) and the crew, specially [@pharkmillups](http://twitter.com/#!/pharkmillups) for being an awesome community manager.

## License ##

See LICENSE.md