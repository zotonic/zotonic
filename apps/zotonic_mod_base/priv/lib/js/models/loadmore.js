/* Load more Cotonic model.
 * Used to load more data when pressing a button, optionally silently replacing the URL
 *
 * Copyright 2023 Marc Worrell
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

"use strict";

cotonic.ready.then(() => {

    function payload_url(msg) {
        let url;

        if (msg.payload.url) {
            url = msg.payload.url;
        } else if (typeof msg.payload.message == "object") {
            url = msg.payload.message['data-url'] ?? msg.payload.message.href;
        }
        return url;
    }

    function searchParamsList( qs ) {
        let ps = [];

        const searchParams = new URLSearchParams(qs);
        searchParams.forEach((value, key) => {
            ps.push([key, value]);
        });
        return ps;
    }

    function is_replace_location(msg) {
        const r = msg.payload.replace_location
                  ?? msg.payload?.message['data-replace-location']
                  ?? true;

        if (typeof r === "string") {
            switch (r.toLowerCase()) {
                case "":
                case "0":
                case "false":
                case "no":
                    return false;
                default:
                    return true;
            }
        } else {
            return !!r;
        }
    }

    cotonic.broker.subscribe("model/loadmore/post/replace", (msg) => {
        let target;
        let template;

        if (msg.payload.message && msg.payload.message.id) {
            target = msg.payload.message.id;
            template = msg.payload.message["data-template"] ?? msg.payload.message.template;
        } else if (msg.payload.id) {
            target = msg.payload.id;
            template = msg.payload.template;
        }

        if (target && template) {
            const element = document.getElementById(target);
            if (!element) {
                return;
            }
            element.classList.add("loading");

            const url = payload_url(msg);
            let qargs;

            if (url) {
                const urlParsed = new URL(url, window.location);
                qargs = searchParamsList(urlParsed.search);
            } else if (msg.payload.qargs) {
                qargs = msg.payload.qargs;
            } else if (msg.payload.message) {
                qargs = msg.payload.message;
            } else {
                qargs = msg.payload;
            }

            if (url && is_replace_location(msg)) {
                cotonic.broker.publish("model/location/post/replace-silent", url);
            }
            cotonic.broker.publish(
                "bridge/origin/model/template/get/render/" + template,
                qargs,
                {
                    properties: {
                        response_topic: "model/ui/replace/" + target
                    },
                    qos: 1
                })
        } else {
            console.log("'model/loadmore/post/replace' missing target or template", msg);
        }
    });

});
