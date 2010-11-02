<html>
    <title>{_ Image clipper _}</title>
    <style type="text/css">
        #progress div {
        padding: 2px;
        border: 1px solid #e0e0e0;
        margin: 10px;
        float: left;
        background: url(/lib/images/spinner.gif) no-repeat center;
        }
    </style>
    <body>

        <div id="progress">
            {% for a in aspects %}
            <div id="img-{{ forloop.counter0 }}" style="height: 100px; width: {{ a*100 }}px">
            </div>
            {% endfor %}
        </div>
        {% include "_js_include_jquery.tpl" %}
        {% lib
		"js/apps/zotonic-1.0.js"
		"js/modules/jquery.loadmask.js"
        %}
        
        {# Initialize the zotonic postback #}
        {% stream %}
        {% script %}

        <script type="text/javascript">
            $(function() 
            { 
            z_queue_postback("progress", "{{ postback }}", [], true);
            });
        </script>


    </body>

</html>
