/* live js
----------------------------------------------------------

@package:   Zotonic 2014-2024
@Author:    Marc Worrell <marc@worrell.nl>

Copyright 2014-2024 Marc Worrell

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
    this._timers = [];
    this._wid = 0;
    const self = this;
    setInterval(function() { self.prune(); }, 10000);
}

ZLive.prototype.subscribe = function(topics, target, isUiInsert, postback) {
    const self = this;

    for(let i = topics.length-1; i >= 0; i--) {
        const topic = topics[i];
        const wid = '-z-live-' + self._wid++;

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
            postback: postback,
            is_widget: false
        });

        if (isUiInsert) {
            cotonic.broker.publish("model/ui/insert/" + target, {});
        }
    }
};

ZLive.prototype.subscribeWidget = function($element, options) {
    const self = this;
    let template = options.template;
    let topics = options.topics;

    z_ensure_id($element);

    if (!template) {
        template = $element.attr('data-template');
    }
    if (!topics || (Array.isArray(topics) && !topics.length)) {
        topics = $element.attr('data-live-topic');
    }

    if (typeof(topics) == 'string') {
        topics = topics.split(";");
    }

    if (Array.isArray(topics) && topics.length && template) {
        const target = $element.attr('id');
        options.template = template;

        for(let i = topics.length-1; i >= 0; i--) {
            const topic = topics[i];
            const wid = 'z-live-' + self._wid++;

            cotonic.broker.subscribe(
                topic,
                function(msg, _mapping, opts) {
                    self.updateWidget(topic, target, options, msg, opts.wid);
                },
                { wid: wid });

            this._subscriptions.push({
                wid: wid,
                topic: topic,
                target: target,
                postback: undefined,
                is_widget: true
            });
        }
    }
};

ZLive.prototype.update = function(topic, target, postback, message, wid) {
    if (document.getElementById(target)) {
        const dedup_key = target;
        const extraParams = {
            topic: topic,
            message: message
        };
        z_queue_postback(target, postback, extraParams, undefined, undefined, undefined, { dedup_key: dedup_key });
    } else {
        this.unsubscribe(wid);
    }
};

ZLive.prototype.updateWidget = function(topic, target, options, message, wid) {
    if (document.getElementById(target)) {
        // Wait till 100 msecs passes without any triggererd events
        if (this._timers[wid]) {
            clearTimeout(this._timers[wid]);
        }
        const self = this;
        this._timers[wid] = setTimeout(
            function() {
                const payload = {
                    topic: topic,
                    target: target,
                    message: message,
                    data: document.getElementById(target).dataset
                };
                cotonic.broker.publish(
                    "bridge/origin/model/template/get/render/" + options.template,
                    payload,
                    {
                        properties: {
                            response_topic: "model/ui/replace/" + target
                        },
                        qos: 0
                    });
                self._timers[wid] = undefined;
            },
            100);
    } else {
        this.unsubscribe(wid);
    }
};

ZLive.prototype.unsubscribe = function(wid) {
    for (let i = this._subscriptions.length-1; i >= 0; i--) {
        if (this._subscriptions[i].wid == wid) {
            cotonic.broker.unsubscribe(this._subscriptions[i].topic, { wid: wid });
            this._subscriptions.splice(i,1);
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


/* Define the `do_live` widget */
$.widget("ui.live",
{
    _init: function()
    {
        const $elt = this.element;
        window.z_live.subscribeWidget(this.element, this.options);
    }
});

$.ui.live.defaults = {
    topics: [],
    template: ""
};
