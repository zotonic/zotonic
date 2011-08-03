{% extends "admin_base.tpl" %}

{% block title %}{_ Pages _}{% endblock %}

{% block content %}

	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>Page Overview</h2>
		<div class="clearfix">
			{% all include "_admin_make_page_buttons.tpl" %}
			{% button class="" text=_"Make a new page" action={dialog_new_rsc title="" cat=q.qcat} %}
			{% button class="" text=_"Make a new media item" action={dialog_media_upload title=""} %}
		</div>
		
		<hr />
		
			<form id="{{ #form }}" method="GET" action="{% url admin_overview_rsc qs=q.qs %}">
				<input type="hidden" name="qsort" value="{{ q.qsort }}" />
				<input type="hidden" name="qs" value="{{ q.qs }}" />
				<h3 class="above-list ">
					{_ Pages overview _}{% if q.qs %}, 
						{_ matching _} “{{ q.qs|escape }}”
						{% button text="show all" action={redirect dispatch="admin_overview_rsc" qcat=q.qcat} %}
						<input type="hidden" name="qs" value="{{ q.qs|escape }}" />
					{% endif %}

					{% with q.qcat as qcat %}
						&mdash; {_ filter on category _}
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
					{% endwith %}
				</h3>
			</form>

            {% with m.search.paged[{query authoritative=1 cat=q.qcat cat_exclude="meta" text=q.qs page=q.page sort=q.qsort|default:"-modified"}] as result %}

            {% catinclude "_admin_overview_list.tpl" q.qcat result=result %}

			{% pager result=result dispatch="admin_overview_rsc" qargs %}

            {% endwith %}

		</div>
	</div>


{% endblock %}
