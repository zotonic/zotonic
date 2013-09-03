/* Zotonic's Pubsub using qmtt over websockets/comet 

Special topics:

    "foo/bar"    within this page
    "/foo/bar"   within the site, handled on the server
    "//foo/bar"  the root of the server's mqtt router

TODO:

    * Server side: add ACL checks
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
}

Pubzub.prototype.me = function () {
    return "page/" + z_pageid;
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
    var will_id = this.unique_id();
    this.transport("lastwill", topic, message, {will_id: will_id});
    return will_id;
};

Pubzub.prototype.is_local_topic = function (topic) {
    return topic.substr(0,1) != "/";
};

Pubzub.prototype.relayed = function (topic, data) {
    console.log("relay", topic, data);
    var subs = this._matcher.match(topic);
    var message = ubf.decode(data);
    message = this.map_record(message);
    for (var i in subs) {
        subs[i](topic, message);
    }
};

Pubzub.prototype.transport = function (cmd, topic, message, args) {
    args = args || {};
    args.topic = topic;
    args.msg = ubf.encode(message);
    args.z_delegate = "mod_mqtt";
    z_notify(cmd, args);
};

Pubzub.prototype.map_record = function (data) {
    if (typeof data == 'object') {
        switch (data[0].valueOf())
        {
            case 'z_mqtt_payload':
                return {
                    version: data[1],
                    site: data[2].valueOf(),
                    user_id: data[3],
                    payload: (data[4] == 'ubf') ? ubf.decode(data[5]) : data[5]
                };
            default:
                return data;
        }
    } else {
        return data;
    }
};

Pubzub.prototype.unique_id = function () {
    return ((new Date()).getTime() + "" + Math.floor(Math.random() * 1000000)).substr(0, 18);
};

(function($) {
    window.pubzub = new Pubzub();
})(jQuery);
