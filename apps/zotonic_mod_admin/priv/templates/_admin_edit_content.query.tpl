{% extends "admin_edit_widget_std.tpl" %}

{# To edit the stored search query #}


{% block widget_title %}
{_ Search query _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
<fieldset>
	<p class="help-block">
		{_ Enter a search query. The query format is detected automatically and checked while you type. _}
	</p>
    <p class="help-block">
        {_ Search queries can use one argument per line or JSON. Other enabled modules can add query formats, such as SPARQL. _}
        {_ For more information, see the <a href="https://zotonic.com/en/latest/developer-guide/search.html#query-arguments">search query documentation</a>. _}
    </p>

    <div class="form-group">
    	<label class="control-label" for="query">{_ Query _}</label>
    	<div>
    	    <textarea class="form-control" id="{{ #query }}" name="query" rows="15" placeholder="cat=text">{{ id.query }}</textarea>
	        <input type="hidden" id="{{ #query_type }}" name="query_type" value="{{ id.query_type }}">
    	</div>
    </div>
    <div class="form-group">
        <a id="{{ #test_query }}" class="btn btn-primary">{_ Test query _}</a>
        {% wire id=#test_query type="click" action={script script="document.queryPreview();"} %}
    </div>
    <div class="form-group" id="{{ #live_group }}">
    	<div class="checkbox">
            <label>
		       <input type="checkbox" id="{{ #live_input }}" name="is_query_live" {% if id.is_query_live %}checked{% endif %}/>
    	       {_ Live query, send notifications when matching items are updated or inserted. _}
    	   </label>
        </div>
    </div>

    <div id="{{ #querypreview }}"></div>

    {% wire name="query-preview"
            postback={query_preview
                rsc_id=id
                target_id=#query
                div_id=#querypreview
                query_type_id=#query_type
                live_group_id=#live_group
                live_input_id=#live_input
            }
            delegate="controller_admin_edit"
    %}

    {% javascript %}
        const queryInput = document.getElementById('{{ #query }}');
        let queryPreviewTimer;

        document.queryPreview = function() {
            window.clearTimeout(queryPreviewTimer);
            const text = queryInput.value;
            z_event("query-preview", { query: text });
        };

        queryInput.addEventListener("input", function() {
            window.clearTimeout(queryPreviewTimer);
            queryPreviewTimer = window.setTimeout(document.queryPreview, 500);
        });
        setTimeout(document.queryPreview, 100);
    {% endjavascript %}
</fieldset>
{% endblock %}
