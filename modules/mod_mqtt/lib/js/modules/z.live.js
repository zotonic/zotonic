/* live js
----------------------------------------------------------

@package:   Zotonic 2014    
@Author:    Marc Worrell <marc@worrell.nl>

Copyright 2014 Marc Worrell

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
}

ZLive.prototype.subscribe = function(topics, target, postback) {
    var self = this;
    for(var i=topics.length-1; i >= 0; i--) {
        var topic = topics[i];
        var sub_id = pubzub.subscribe(
                            topic,
                            function(_topic, msg, id) {
                                self.update(topic, target, postback, msg, id);
                            });

        this._subscriptions.push({
            sub_id: sub_id,
            topic: topic,
            target: target,
            postback: postback
        });
    }
};

ZLive.prototype.update = function(topic, target, postback, payload, sub_id) {
    if ($('#'+target).length) {
        if (typeof payload._record != 'undefined' && payload._record == 'z_mqtt_payload') {
            payload = payload.payload;
        }
        z_queue_postback(target, postback, {topic: topic, message: payload});
    } else {
        for (i=0; i<= this._subscriptions.length; i++) {
            if (this._subscriptions[i].sub_id == sub_id) {
                this._subscriptions.splice(i,1);
                break;
            }
        }
        pubzub.unsubscribe(sub_id);
    }
};

window.z_live = new ZLive();
