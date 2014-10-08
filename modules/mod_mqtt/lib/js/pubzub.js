/* Zotonic's Pubsub using qmtt over websockets/comet 

Special topics:

    "~pagesession"    within this page
    "~site/foo/bar"   within the site, handled on the server
    "foo/bar"  the root of the server's mqtt router

TODO:

    * add relay from page topics to server topics
    * add QoS levels
        - default level 0
        - add level 0 and 1, needs local puback
        - add dup flag to messages 
    * retain local messages for topics
*/

function Pubzub ()
{
    this._matcher = new Qlobber({
        separator: '/',
        wildcard_one: '+',
        wildcard_some: '#'
    });
    this._subs = {};
    this._subs_relayed = {};

    this._will_id = 0;
    ubf.add_spec("mqtt_msg", [
            "retain", "qos", "topic", "dup", "message_id", "payload",
            "encoder"
        ]);
    ubf.add_spec("z_mqtt_payload", [
            "site", "user_id", "payload"
        ]);
    ubf.add_spec("z_mqtt_cmd", [
            "cmd", "topic", "payload", "extra"
        ]);

    var self = this;
    z_transport_delegate_register(
            'mqtt_route',
            function(mqtt_msg, msg) {
                self.relayed(mqtt_msg, msg);
            });
}

Pubzub.prototype.me = function () {
    return "~site/pagesession/" + z_pageid;
};

Pubzub.prototype.subscribe_multi = function (topics, fun) {
    for(var i=topics.length-1; i >= 0; i--) {
        this.subscribe(topics[i], fun);
    }
};

Pubzub.prototype.subscribe = function (topic, fun) {
    var id = this.unique_id();
    this._matcher.add(topic, id);
    this._subs[id] = {fun: fun, topic: topic};
    if (!this.is_local_topic(topic)) {
        var ct = this._subs_relayed[topic] || 0;
        if (ct === 0) {
            this.transport("subscribe", topic);
        }
        this._subs_relayed[topic] = ct + 1;
    }
    return id;
};

Pubzub.prototype.unsubscribe = function (id) {
    if (this._subs[id]) {
        var sub = this._subs[id];
        var topic = sub.topic;

        this._matcher.remove(topic, sub.id);
        if (!this.is_local_topic(topic)) {
            var ct = this._subs_relayed[topic] - 1;
            if (ct === 0) {
                this.transport("unsubscribe", topic);
            }
            this._subs_relayed[topic] = ct;
        }
        delete this._subs[id];
    }
};

Pubzub.prototype.publish = function (topic, message) {
    if (this.is_local_topic(topic)) {
        var subs = this._matcher.match(topic);
        for (var i in subs) {
            var id = subs[i];
            if (this._subs[id]) {
                this._subs[id].fun(topic, message, id);
            }
        }
    } else {
        this.transport("publish", topic, message);
    }
};

Pubzub.prototype.subscribers = function (topic) {
    return this._matcher.match(topic);
};

Pubzub.prototype.lastwill = function (topic, message) {
    this._will_id = this.unique_id();
    this.transport("lastwill", topic, message, this._will_id);
};

Pubzub.prototype.remove_lastwill = function () {
    this.transport("lastwill", undefined, undefined, this._will_id);
    this._will_id = undefined;
};

Pubzub.prototype.is_local_topic = function (topic) {
    var me = this.me();

    return topic == '~pagesession' ||
           topic == me ||
           topic.substr(0,13) == "~pagesession/" ||
           topic.substr(0,me.length+1) == me+"/";
};

Pubzub.prototype.relayed = function (mqtt_msg, _msg) {
    var subs = this._matcher.match(mqtt_msg.topic);
    for (var i in subs) {
        var id = subs[i];
        if (this._subs[id]) {
            this._subs[id].fun(mqtt_msg.topic, mqtt_msg.payload, id);
        }
    }
};

Pubzub.prototype.transport = function (cmd, topic, payload, extra) {
    var msg = {
        _record: 'z_mqtt_cmd',
        cmd: cmd,
        topic: topic,
        payload: payload,
        extra: extra
    };
    var options = {
        qos: 1
    };
    z_transport('mqtt', 'ubf', msg, options);
};

Pubzub.prototype.unique_id = function () {
    var t = (new Date()).getTime() + "-";
    var cs = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    for( var i=0; i < 20; i++ )
        t += cs.charAt(Math.floor(Math.random() * cs.length));
    return t;
};

(function($) {
    window.pubzub = new Pubzub();
})(jQuery);
