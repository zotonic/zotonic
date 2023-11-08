/* live js
----------------------------------------------------------

@package:   Zotonic 2014-2023
@Author:    Marc Worrell <marc@worrell.nl>

Copyright 2014-2023 Marc Worrell

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
    this._wid = 0;
    const self = this;
    setInterval(function() { self.prune(); }, 10000);
}

ZLive.prototype.subscribe = function(topics, target, isUiInsert, postback) {
    for(let i = topics.length-1; i >= 0; i--) {
        const self = this;
        const topic = topics[i];
        const wid = 'z-live-' + self._wid++;

        cotonic.broker.subscribe(
            topic,
            function(msg, _mapping, opts) {
                self.update(opts.topic, target, postback, msg, opts.wid);
            },
            { wid: wid });

        this._subscriptions.push({
            wid: wid,
            topic: topic,
            target: target,
            postback: postback
        });

        if (isUiInsert) {
            cotonic.broker.publish("model/ui/insert/" + target, {});
        }
    }
};

ZLive.prototype.update = function(topic, target, postback, payload, wid) {
    if (document.getElementById(target)) {
        const dedup_key = target;
        const extraParams = {
            topic: topic,
            message: payload
        };
        z_queue_postback(target, postback, extraParams, undefined, undefined, undefined, { dedup_key: dedup_key });
    } else {
        this.unsubscribe(topic, { wid: wid});
    }
};

ZLive.prototype.unsubscribe = function(wid) {
    for (let i = 0; i < this._subscriptions.length; i++) {
        if (this._subscriptions[i].wid == wid) {
            cotonic.broker.unsubscribe(this._subscriptions[i].topic, { wid: wid });
            this._subscriptions.splice(i,1);
            break;
        }
    }
};

ZLive.prototype.prune = function() {
    for (let i = this._subscriptions.length-1; i >= 0; i--) {
        const target = this._subscriptions[i].target;

        if (!document.getElementById(target)) {
            cotonic.broker.unsubscribe(this._subscriptions[i].topic, { wid: this._subscriptions[i].wid });
            this._subscriptions.splice(i,1);
        }
    }
};

window.z_live = new ZLive();
