(function($) {
    var RabbitMQChat = function() {
        this.ws = null;
        this.since = new Date().getTime() - (60 * 60 * 1000); // Scope to the last hour initially
        this.clientId = "js_" + $.base64.encode(Math.floor(Math.random() * 4294967296));
        var that = this;
        window.onunload = function() {
            if(that.ws) {
                that.ws.close();
            }
        }
    };

    RabbitMQChat.prototype.connect = function() {
        if ("WebSocket" in window) {
          // browser supports websockets
          this.ws = new WebSocket("ws://" + window.location.host + "/service");
          var that = this;
          this.ws.onopen = function() {
              // websocket is connected
              that.toggleConnStatus('connected');
          };
          this.ws.onmessage = function (evt) {
              var obj = JSON.parse(evt.data);
              that.displayMessage(obj);
          };
          this.ws.onclose = function() {
              // websocket was closed
              that.toggleConnStatus('disconnected');
              this.displayError("You got disconnected from the server");
          };

        } else {
            // browser does not support websockets
            this.displayError("Sorry, your browser does not support websockets.");
        }
    };

    RabbitMQChat.prototype.displayError = function(msg) {
        alert(msg);
    };

    RabbitMQChat.prototype.toggleConnStatus = function(status) {
        console.log(status);
    };

    RabbitMQChat.prototype.displayMessage = function(item) {
        if($('#' + item.key).length == 0) {
            var elem = $('<li id="' + item.key + '" />');
            var avatar = $('<img />').attr('src', 'http://gravatar.com/avatar/' + item.gravatar + '?s=40');
            var name = $('<span class="name">').html(item.name);
            var message = $('<span class="message">').html(item.message);
            var timestamp = $('<span class="timestamp">').text(new Date(item.timestamp).toLocaleTimeString());
            elem.append(timestamp).append(avatar).append(name).append(message);
            if(item.name == this.name && item.gravatar == this.gravatar) {
                elem.addClass('me');
            }
            $('ol#chatlog').append(elem).attr({ scrollTop: $('ol#chatlog').attr("scrollHeight") });
        }
    };

    RabbitMQChat.prototype.postMessage = function(message) {
        message = $.trim(message);
        if(message.length > 0) {
            var key = hex_md5(this.clientId + new Date().toString());
            msg = {
                'key': key,
                'message': this.escape(message),
                'name': this.escape(this.name),
                'gravatar': this.gravatar,
                'timestamp': new Date().getTime()
            };
            this.ws.send(JSON.stringify(msg));
            this.displayMessage(msg);
        }
        $('form#chatbox').get(0).reset();
    };

    RabbitMQChat.prototype.escape = function(string) {
        return string.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    };

    var COOKIE_NAME = 'rmqchat.name';
    var COOKIE_GRAVATAR = 'rmqchat.gravatar';

    RabbitMQChat.prototype.start = function(name, email) {
        this.name = name;
        this.gravatar = (email && email.indexOf('@') != -1) ? hex_md5(email) : email;
        if($.trim(this.name).length != 0) {
            if(Cookie.accept()) {
                Cookie.set(COOKIE_NAME, this.name, 4);
                Cookie.set(COOKIE_GRAVATAR, this.gravatar, 4);
            }
            $('form#login').hide();
            $('ol#chatlog, form#chatbox').show();
            this.connect();
        } else {
            alert("Please enter a name for yourself. An email would be nice too (not sent over the wire).");
        }
    };

    $(document).ready(function() {
        var rmqchat = new RabbitMQChat();
        if(Cookie.accept()) {
            var name = Cookie.get(COOKIE_NAME);
            var gravatar = Cookie.get(COOKIE_GRAVATAR);
            if(name) {
                rmqchat.start(name, gravatar);
            }
        }
        $('form#login').submit(function(e) {
            e.preventDefault();
            rmqchat.start($('#name').val(), $('#email').val());
            return false;
        });
        $('form#chatbox').submit(function(e) {
            e.preventDefault();
            rmqchat.postMessage($('#message').val());
        });
    });
})(jQuery);