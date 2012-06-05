{% extends "base.tpl" %}

{% block content %}
{% block above %}{% include "_breadcrumb.tpl" %}{% endblock %}
<div class="row-fluid">
    <div class="span6">
        {% block main %}{% endblock %}
    </div>

    <div id="subnavbar" class="span3">
        {% block subnavbar %}&nbsp;{% endblock %}
    </div>
    
    <aside id="sidebar" class="span3">
        {% block sidebar %}{% include "_sidebar.tpl" %}{% endblock %}
    </aside>
</div>
{% block below %}{% endblock %}
{% endblock %}

{% block related %}
{% include "_content_list.tpl" list=id.o.hasdocument title=_"Documents"%}

{% with id.o.haspart, id.s.haspart as sub,super %}
{% if sub or super %}
	<h3>{_ Related _}</h3>
    {% include "_content_list.tpl" list=sub in_collection=id %}
    {% include "_content_list.tpl" list=super %}
{% endif %}
{% endwith %}
{% endblock %}