{% if m.config.mod_development.livereload.value %}
    {% lib "js/livereload.js" %}

    <script type="text/javascript">
        window.addEventListener(
            "load",
            function() {
                setTimeout(
                    function() {
                        pubzub.subscribe("public/development/livereload", function (topic, msg) {
                            if (msg.payload.is_page_reload) {
                                z_reload();
                            } else {
                                setTimeout(
                                    function() {
                                        zotonic_live_reloader.reload.apply(zotonic_live_reloader, [ msg.payload.path, { liveCSS: true } ]);
                                    }, 1000);
                                setTimeout(
                                    function() {
                                        zotonic_live_reloader.reload.apply(zotonic_live_reloader, [ msg.payload.path, { liveCSS: true } ]);
                                    }, 2000);
                            }
                        });
                    }, 100);
            });
    </script>
{% endif %}