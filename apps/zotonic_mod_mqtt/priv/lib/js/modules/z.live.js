/* live js
----------------------------------------------------------

@package:   Zotonic 2014-2018
@Author:    Marc Worrell <marc@worrell.nl>

Copyright 2014-2018 Marc Worrell

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---------------------------------------------------------- */

function ZLive ()
{
    this._subscriptions = [];
    this._prune_timeout = 10000;
    var self = this;
    setTimeout(function() { self.prune(); }, this._prune_timeout);
}

ZLive.prototype.subscribe = function(topics, target, postback) {
    var self = this;
    for(var i=topics.length-1; i >= 0; i--) {
        var topic = topics[i];
        var wid = cotonic.broker.subscribe(
                            topic,
                            function(msg, _mapping, opts) {
                                self.update(opts.topic, target, postback, msg, opts.wid);
                            });

        this._subscriptions.push({
            wid: wid,
            topic: topic,
            target: target,
            postback: postback
        });
    }
};

ZLive.prototype.update = function(topic, target, postback, payload, wid) {
    if ($('#'+target).length) {
        if (typeof payload._record != 'undefined' && payload._record == 'z_mqtt_payload') {
            payload = payload.payload;
        }
        z_queue_postback(target, postback, {topic: topic, message: payload});
    } else {
        this.unsubscribe(topic, { wid: wid});
    }
};

ZLive.prototype.unsubscribe = function(wid) {
    for (var i=0; i<= this._subscriptions.length; i++) {
        if (this._subscriptions[i].wid == wid) {
            cotonic.broker.unsubscribe(this._subscriptions[i].topic, { wid: wid });
            this._subscriptions.splice(i,1);
            break;
        }
    }
};

ZLive.prototype.prune = function() {
    for (var i=this._subscriptions.length-1; i >= 0; i--) {
        var target = this._subscriptions[i].target;
        if ($('#'+target).length === 0) {
            cotonic.broker.unsubscribe(this._subscriptions[i].topic, { wid: this._subscriptions[i].wid });
            this._subscriptions.splice(i,1);
        }
    }
    var self = this;
    setTimeout(function() { self.prune(); }, this._prune_timeout);
};

window.z_live = new ZLive();
