<html>
    <head>
        <title>Test Initial Postback</title>
        {% include "_html_head_cotonic.tpl" %}
    </head>

    <body>

        {% with 100 as till %}
            <h1>Counter should reach {{ till }}</h1>

            <div id="update">
                {% with q.n|default:0|to_integer as n %}
                    <p>Requesting: {{ n+1 }}</p>
                    {% wire action={update
                                        target="update"
                                        template="tests/_initial_postback_test.tpl"
                                        n=n+1
                                        till=till
                                } %}
                {% endwith %}

                <p>
                    <a href="/test/initial_postback_test">Restart</a>
                </p>
            </div>
        {% endwith %}

        {% include "_js_include_jquery.tpl" %}

        {% lib
            "js/modules/jstz.min.js"
            "cotonic/cotonic.js"
            "js/apps/zotonic-wired.js"
            "js/apps/z.widgetmanager.js"
            "js/modules/jquery.loadmask.js"
        %}

        <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
            $(function()
            {
                $.widgetManager();
            });
        </script>

        {% script %}

        {% worker name="auth" src="js/zotonic.auth.worker.js" args=%{  auth: m.authentication.status  } %}
    </body>
</html>
