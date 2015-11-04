<div class="rsc-item" id="{{ #item }}">
   	{% image id.depiction mediaclass="admin-list-overview" class="thumb pull-left" %}
	<strong><a href="{% url admin_edit_rsc id=id %}">{% include "_name.tpl" %}</a></strong><br />
	{# <span class="text-muted">{{ id|summary:50 }}</span> #}
    <div class="text-muted">
        {% catinclude "_admin_overview_list_data.tpl" id %}
    </div>
</div>
