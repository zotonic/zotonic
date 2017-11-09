{% extends "admin_base.tpl" %}

{% block content %}
{% with m.rsc[q.name].id as id %}
	{% if id.is_a.category %}
		<div class="admin-header">
			<h2>{_ Hierarchy for _}: {{ id.title }}</h2>
		<div>

		<div class="row">
			{# For now demand mod_admin_config rights #}
			{% if m.acl.use.mod_admin_config %}
                {% with m.acl.is_allowed.insert[id.name] as editable %}
                    <div id="{{ #sorter }}" class="col-lg-8 col-md-8">

                            <div class="clearfix panel">
                                {% if editable %}
                                <div class="btn-group pull-right">
                                    <a href="#" class="btn btn-default dropdown-toggle" data-toggle="dropdown">{_ Add _} {{ id.title }} <span class="caret"></span></a>
                                    <ul class="dropdown-menu">
                                        <!-- dropdown menu links -->
                                        <li><a href="#" data-where="top">&uarr; {_ Add top _}</a></li>
                                        <li><a href="#" data-where="bottom">&darr; {_ Add bottom _}</a></li>
                                    </ul>
                                </div>
                                {% endif %}
                            </div>

                        {% include "_admin_menu_hierarchy_sorter.tpl"
                                    sorter=#sorter name=id.name
                                    editable=editable
                                    cat_id=id is_unique_ids
                        %}
                    </div>

                    <div id="sidebar" class="col-lg-4 col-md-4">
                        <div class="widget">
                            <h3 class="widget-header">{_ How does this work? _}</h3>
                            <div class="widget-content">
                                <p>
                                    {_ Create an (optionally) nested navigation menu for this site. _}
                                </p>
                                <p>
                                    {_ Drag pages to the place where you want them in the hierarchy. _}
                                </p>
                            </div>
                        </div>
                    </div>
                {% endwith %}
			{% else %}
				<ul class="list-group">
					{% for cg in m.hierarchy[id.name].tree_flat %}
						<li class="list-group-item">{{ cg.indent }} {{ cg.id.title }}</li>
					{% empty %}
						<li class="list-group-item">
							<div class="alert alert-info">{_ Empty hierarchy _}</div>
						</li>
					{% endfor %}
				</ul>
			{% endif %}
		</div>
	{% else %}
		{# some random hierarchy - later we could support creating any kind of hierarchy #}
		{# hierarchies are more formal menu definitions, which is useful for database access #}
		<div class="alert alert-error" role="alert">
			<strong>{_ Unknown category _}: {{ q.name|escape }}</strong>
			{_ Hierarchies can be defined for any existing category. _}
		</div>
	{% endif %}
{% endwith %}
{% endblock %}
