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
                                z_reload();
                            } else {
                                zotonic_live_reloader.reload.apply(zotonic_live_reloader, [ msg.payload.path, { liveCSS: true } ]);
                            }
                        });
                    }, 100);
            });
    </script>
{% endif %}
{% if m.development.show_trace_button %}
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
                    }, 100);
            }
        );
    </script>
    <style type="text/css" nonce="{{ m.req.csp_nonce }}">
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
