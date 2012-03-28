{% extends "admin_base.tpl" %}

{% block title %}{_ Pages _}{% endblock %}

{% block content %}

<form id="{{ #form }}" method="GET" action="{% url admin_overview_rsc qs=q.qs %}" class="form-horizontal">
    <div class="pull-right">
	{% with q.qcat as qcat %}
        <div class="control-group">
	    <label class="control-label" for="{{ #category }}">{_ Filter on category _}</label>
            <div class="controls">
	        <select id="{{ #category }}" name="qcat">
	            <option value="">{_ All Categories _}</option>
	            <option disabled="disabled"></option>
	            {% for cat_id, level, indent, name in m.category.all_flat %}
	            {% if m.acl.insert[name|as_atom] %}
	            <option value="{{ name }}" {% ifequal name qcat %}selected="selected" {% endifequal %}>
		        {{ indent }}{{ m.rsc[cat_id].title|default:name }}
	            </option>
	            {% endif %}
	            {% endfor %}
	        </select>
	        {% wire type="change" id=#category action={submit} %}
            </div>
        </div>
	{% endwith %}
    </div>

    <input type="hidden" name="qsort" value="{{ q.qsort }}" />
    <input type="hidden" name="qs" value="{{ q.qs }}" />
    <h2>
	{_ Pages overview _}{% if q.qcat %}: {{ m.rsc[q.qcat].title }}{% endif %}{% if q.qs %}, 
	{_ matching _} “{{ q.qs|escape }}”
	{% button text="show all" class="btn btn-mini" action={redirect dispatch="admin_overview_rsc" qcat=q.qcat} %}
	<input type="hidden" name="qs" value="{{ q.qs|escape }}" />
	{% endif %}
    </h2>
</form>

<div class="well">
    {% all include "_admin_make_page_buttons.tpl" %}
    {% button class="btn btn-primary" text=_"Make a new page" action={dialog_new_rsc title="" cat=q.qcat} %}
    {% button class="btn" text=_"Make a new media item" action={dialog_media_upload title=""} %}
</div>
		
{% with m.search.paged[{query authoritative=1 cat=q.qcat cat_exclude="meta" text=q.qs page=q.page sort=q.qsort|default:"-modified"}] as result %}
        {% catinclude "_admin_overview_list.tpl" q.qcat result=result %}
	{% pager result=result dispatch="admin_overview_rsc" qargs hide_single_page=1 %}
{% endwith %}

{% endblock %}
