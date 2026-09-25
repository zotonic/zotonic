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

ZLive.prototype.subscribe = function(topics, target, isUiInsert, postback, throttle) {
    const self = this;
    const interval = Number(throttle);
    // Share the interval across all topics of this live element.
    const refresh = {
        timer: undefined,
        pending: undefined,
        lastEvent: -Infinity,
        lastRefresh: -Infinity
    };
    const update = function(topic, message, wid) {
        if (!Number.isFinite(interval) || interval <= 0) {
            self.update(topic, target, postback, message, wid);
            return;
        }
        const now = performance.now();
        const wasIdle = now - refresh.lastEvent >= interval;
        refresh.lastEvent = now;
        refresh.pending = { topic, message, wid };
        if (refresh.timer === undefined) {
            // Briefly combine the initial burst, then measure the full interval
            // from the refresh itself. An idle subscription starts quickly again.
            const delay = wasIdle
                ? Math.min(100, interval)
                : Math.max(0, interval - (now - refresh.lastRefresh));
            refresh.timer = setTimeout(function() {
                const pending = refresh.pending;
                refresh.timer = undefined;
                refresh.pending = undefined;
                refresh.lastRefresh = performance.now();
                self.update(pending.topic, target, postback, pending.message, pending.wid);
            }, delay);
        }
    };

    for(let i = topics.length-1; i >= 0; i--) {
        const topic = topics[i];
        const wid = '-z-live-' + self._wid++;

        cotonic.broker.subscribe(
            topic,
            function(msg, _mapping, opts) {
                update(opts.topic, msg, opts.wid);
            },
            { wid: wid });

        this._subscriptions.push({
            wid: wid,
            topic: topic,
            target: target,
            postback: postback,
            is_widget: false,
            refresh: refresh
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
                const targetElt = document.getElementById(target);
                if (targetElt) {
                    const payload = {
                        topic: topic,
                        target: target,
                        message: message,
                        data: targetElt.dataset
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
                }
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
            const refresh = this._subscriptions[i].refresh;
            if (refresh) {
                clearTimeout(refresh.timer);
                refresh.timer = undefined;
                refresh.pending = undefined;
            }
            cotonic.broker.unsubscribe(this._subscriptions[i].topic, { wid: wid });
            this._subscriptions.splice(i,1);
        }
    }
};

ZLive.prototype.prune = function() {
    for (let i = this._subscriptions.length-1; i >= 0; i--) {
        const target = this._subscriptions[i].target;

        if (!document.getElementById(target)) {
            this.unsubscribe(this._subscriptions[i].wid);
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
