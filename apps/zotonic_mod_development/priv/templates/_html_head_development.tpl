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
