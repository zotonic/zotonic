{% if m.acl.is_allowed.use.mod_development %}
    {% lib "js/development-editor.js" %}
{% endif %}
{% if m.development_capture.console_allowed %}
    {% lib "js/development-tools.js" %}
{% endif %}
{% if m.development.livereload %}
    {% lib "js/livereload.js" %}

    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
        window.addEventListener(
            "load",
            function() {
                setTimeout(
                    function() {
                        cotonic.broker.subscribe("bridge/origin/public/development/livereload", function (msg) {
                            if (msg.payload.is_page_reload) {
                                {% if zotonic_dispatch_module /= `mod_development` %}
                                    z_reload();
                                {% endif %}
                            } else {
                                zotonic_live_reloader.reload.apply(zotonic_live_reloader, [ msg.payload.path, { liveCSS: true } ]);
                            }
                        });
                    }, 100);
            });
    </script>
{% endif %}
{% if m.development.show_trace_button %}
    {% if m.development_capture.console_allowed %}
        {% lib "js/development-capture.js" %}
    {% endif %}
    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
        window.addEventListener(
            "load",
            () => {
                setTimeout(
                    () => {
                        const a = document.createElement("a");
                        a.id = "debug-trace-btn";
                        a.target = "debug";
                        a.href = "{% url admin_development_templates_trace sid='sid' path=m.req.raw_path %}".replace("&amp;", "&");
                        a.innerText = "🪰";
                        a.title = "{_ Trace templates _}"
                        a.ariaLabel = "{_ Trace templates _}"
                        document.body.appendChild(a);
                        {% if m.development_capture.console_allowed %}
                            window.z_development_capture.install(a, {
                                trace: "{_ Trace templates _}",
                                attach: "{_ Attach capture _}",
                                stop: "{_ Stop capture _}"
                            });
                        {% endif %}
                    }, 100);
            }
        );
    </script>
    <style type="text/css" nonce="{{ m.req.csp_nonce }}">
        #development-capture-panel {
            position: fixed;
            bottom: 48px;
            right: 8px;
            z-index: 99999;
            padding: 12px;
            background: white;
            color: black;
            border: 1px solid #888;
        }
        #development-capture-panel > * { margin: 4px; }
        #development-capture-panel output { display: block; user-select: all; }
        #debug-trace-btn {
            position: fixed;
            bottom: 8px;
            right: 8px;
            height: 32px;
            width: 32px;
            z-index: 99999;
            display: table-cell;
            text-align: center;
            vertical-align: middle;
            border: 2px solid red;
            border-radius: 16px;
            font-size: 22px;
            line-height: 1.2;
            background-color: white;
            opacity: 0.3;
            color: white;
            box-shadow: 0 0 4px rgba(0,0,0,0.5);
            text-decoration: none;
        }

        #debug-trace-btn:hover {
            opacity: 1;
        }
    </style>
{% endif %}
