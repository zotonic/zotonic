{% extends "admin_edit_widget_std.tpl" %}

{# To edit the stored search query #}


{% block widget_title %}
{_ Search query _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
<fieldset>
	<p class="notification notice">
		{_ Here you can edit the arguments of the search query. Every argument goes on its own line. For more information, see the <a href="https://zotonic.com/en/latest/developer-guide/search.html#query-arguments">documentation on the query arguments</a> on the Zotonic website. _}
	</p>

    <div class="form-group">
    	<label class="control-label" for="query">{_ Query _}</label>
    	<div>
    	    {% with "cat='text'" as placeholder %}
    	    <textarea class="form-control" id="{{ #query }}" name="query" rows="15" placeholder="{{ placeholder }}">{{ id.query }}</textarea>
    	    {% endwith %}
    		{% wire id=#query type="change" postback={query_preview rsc_id=id div_id=#querypreview target_id=#query} delegate="controller_admin_edit" %}
    	</div>
    </div>
    <div class="form-group">
        <a id="{{ #test_query }}" class="btn btn-default">{_ Test query _}</a>
        {% wire id=#test_query type="click" action={script script="$('#query').trigger('change')"} %}
    </div>

    <div class="form-group">
    	<div class="checkbox"><label>
    	    <input type="checkbox" id="is_query_live" name="is_query_live" {% if id.is_query_live %}checked{% endif %}/>
    	    {_ Live query, send notifications when matching items are updated or inserted. _}
    	</label></div>
    </div>

	<h4>{_ Query preview _}</h4>

	<div class="query-results" id="{{ #querypreview }}">
		{% include "_admin_query_preview.tpl" result=m.search[{query query_id=id pagelen=20}] %}
	</div>
</fieldset>
{% endblock %}
