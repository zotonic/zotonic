{% extends "admin_base.tpl" %}

{% block title %}{_ Pages _}{% endblock %}

{% block content %}

{% with q.qpagelen|default:20 as qpagelen %}
<form id="{{ #form }}" method="GET" action="{% url admin_overview_rsc qs=q.qs %}" class="form-horizontal">
    <div class="pull-right">
        <select id="{{ #pagelen }}" name="qpagelen" style="width: auto; margin-left: .75em">
            {% for pagelen_value,pagelen_label in [[10,"10"],[20,"20"],[50,"50"],[100,"100"],[200,"200"],[500,"500"]] %}
            <option value="{{ pagelen_value }}" {% ifequal pagelen_value qpagelen %}selected="selected" {% endifequal %}>
                {{ pagelen_label }}
            </option>
            {% endfor %}
        </select>
        {% wire type="change" id=#pagelen action={submit} %}
    </div>
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
	{% button text=_"show all" class="btn btn-mini" action={redirect dispatch="admin_overview_rsc" qcat=q.qcat} %}
	<input type="hidden" name="qs" value="{{ q.qs|escape }}" />
	{% endif %}
    </h2>
</form>

<div class="well">
    <a name="content-pager"></a>

    {% button   class="btn btn-primary" 
            text=_"Make a new page or media" 
            action={dialog_new_rsc title=""} %}

    {% all include "_admin_make_page_buttons.tpl" %}

    <a class="btn disabled" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
    <a class="btn" href="{% url admin_media %}">{_ All media _}</a>
</div>

{% with m.search.paged[{query authoritative=1 cat=q.qcat text=q.qs page=q.page pagelen=qpagelen sort=q.qsort|default:"-modified"}] as result %}
	{% catinclude "_admin_overview_list.tpl" m.category[q.qcat].is_a result=result %}
	{% pager result=result dispatch="admin_overview_rsc" qargs hide_single_page=1 %}
{% endwith %}
{% endwith %}

{% endblock %}
