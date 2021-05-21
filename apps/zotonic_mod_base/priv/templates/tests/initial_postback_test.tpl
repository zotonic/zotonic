<html>
    <head>
        <title>Test Initial Postback</title>
    </head>

    <body>

        {% with 100 as till %}

            <h1>Counter should reach {{ till }}</h1>

            <div id="update">
                {% with q.n|default:0|to_integer as n %}
                    <p>Requesting: {{ n+1 }}</p>
                    {% wire action={update target="update" template="tests/_initial_postback_test.tpl" n=n+1 till=till} %}
                {% endwith %}

                <p>
                    <a href="/tests/initial_postback_test">Restart</a>
                </p>
            </div>


        {% endwith %}

        {% include "_js_include_jquery.tpl" %}

        {% lib
            "js/modules/jstz.min.js"
            "cotonic/zotonic-wired-bundle.js"
            "js/apps/zotonic-wired.js"
            "js/apps/z.widgetmanager.js"
            "js/modules/jquery.loadmask.js"
        %}

        <script type="text/javascript">
            $(function()
            {
                $.widgetManager();
            });
        </script>

        {% script %}

        {% comment %}{% worker name="auth" src="js/zotonic.auth.worker.js" args=%{  auth: m.authentication.status  } %}{% endcomment %}

    </body>
</html>
