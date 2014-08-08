/* Zotonic's Pubsub using qmtt over websockets/comet 

Special topics:

    "foo/bar"    within this page
    "/foo/bar"   within the site, handled on the server
    "//foo/bar"  the root of the server's mqtt router

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
            function(mqtt_msg, _msg) {
                self.relayed(mqtt_msg, msg);
            });
}

Pubzub.prototype.me = function () {
    return "/page/" + z_pageid;
};

Pubzub.prototype.subscribe = function (topic, fun) {
    this._matcher.add(topic, fun);
    if (!this.is_local_topic(topic)) {
        this.transport("subscribe", topic);
    }
};

Pubzub.prototype.publish = function (topic, message) {
    if (this.is_local_topic(topic)) {
        var subs = this._matcher.match(topic);
        for (var i in subs) {
            subs[i](topic, message);
        }
    } else {
        this.transport("publish", topic, message);
    }
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
    return topic.substr(0,1) != "/";
};

Pubzub.prototype.relayed = function (mqtt_msg, _msg) {
    var subs = this._matcher.match(mqtt_msg.topic);
    for (var i in subs) {
        subs[i](mqtt_msg.topic, mqtt_msg.payload);
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
    // args = args || {};
    // args.topic = topic;
    // args.msg = ubf.encode(message);
    // args.z_delegate = "mod_mqtt";
    // z_notify(cmd, args);
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
