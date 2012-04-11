{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing connections between rscs #}

{% block widget_title %}{_ Page connections _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-connections{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}


<div id="unlink-undo-message" class="clearfix">
    <div class="pull-right">
        <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{{ _"Help about page connections."|escapejs }}', text: '{{ _"This page is able to connect to others. For example you can connect it to an actor or a brand."|escapejs }}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
    </div>
</div>

{% with r.predicates_edit as pred_shown %}
    {% for name, p in m.predicate %}
	{% if p.id|member:pred_shown %}
	    {% ifnotequal name "depiction" %}
	    <h4>
                {{ p.title }}
            </h4>
		
	    <div class="unlink-wrapper clearfix">
		{% sorter id=["links",id|format_integer,name]|join:"-" tag={object_sorter predicate=name id=id} group="edges" handle=".unlink-mover" %}
		<ul id="links-{{ id }}-{{ name }}" class="connections-list">
		    {% for o_id, edge_id in m.edge.o[id][name] %}
		    {% include "_rsc_edge.tpl" subject_id=id predicate=name object_id=o_id edge_id=edge_id %}
		    {% endfor %}
		</ul>
	    </div>
	    {% if is_editable %}
	    <p>{% dialog_link_add subject_id=id predicate=name %}</p>
	    {% endif %}

	    
	    <hr />
	    {% endifnotequal %}
	{% endif %}
    {% endfor %}
{% endwith %}

<div class="button-wrapper clearfix">
	<a href="{% url admin_referrers id=id %}" class="button">{_ View all referrers _}</a>
</div>

{% endwith %}
{% endblock %}
