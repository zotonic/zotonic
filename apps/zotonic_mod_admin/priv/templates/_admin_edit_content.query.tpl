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
		{_ Here you can edit the arguments of the search query. Every argument goes on its own line. For more information, see the <a href="https://zotonic.com/en/latest/developer-guide/search.html#query-arguments">documentation on the query arguments</a> on the Zotonic website. _}
	</p>
    <p class="help-block">
        {_ Define the query using the query arguments or JSON. _}
    </p>

    <div class="form-group">
    	<label class="control-label" for="query">{_ Query _}</label>
    	<div>
    	    <textarea class="form-control" id="{{ #query }}" name="query" rows="15" placeholder="cat='text'">{{ id.query }}</textarea>
    		{% wire id=#query type="change" action={script script="document.queryPreview();"} %}
    	</div>
    </div>
    <div class="form-group">
        <a id="{{ #test_query }}" class="btn btn-default">{_ Test query _}</a>
        {% wire id=#test_query type="click" action={script script="document.queryPreview();"} %}
    </div>
    <div class="form-group">
    	<div class="checkbox">
            <label>
    	       <input type="checkbox" id="is_query_live" name="is_query_live" {% if id.is_query_live %}checked{% endif %}/>
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
            }
            delegate="controller_admin_edit"
    %}

    {% javascript %}
        document.queryPreview = function() {
            const text = document.getElementById('{{ #query }}').value;
            z_event("query-preview", { query: text });
        };
        setTimeout(document.queryPreview, 100);
    {% endjavascript %}
</fieldset>
{% endblock %}
