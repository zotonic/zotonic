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

			<ul class="short-list">
				{% with m.search.paged[{query authoritative=1 cat=q.qcat cat_exclude="meta" text=q.qs page=q.page sort=q.qsort|default:"-modified"}] as result %}

				<li class="headers clearfix">
						<span class="zp-30">
							{% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" %}
						</span>
						<span class="zp-15">
							{% include "_admin_sort_header.tpl" field="category_id" caption=_"Category" %}
						</span>
						<span class="zp-15">
							{% include "_admin_sort_header.tpl" field="created" caption=_"Created on" %}
						</span>
						<span class="zp-15">
							{% include "_admin_sort_header.tpl" field="modified" caption=_"Modified on" %}
						</span>
						<span class="zp-25">
							{% include "_admin_sort_header.tpl" field="modifier_id" caption=_"Modified by" %}
						</span>
				</li>

			{% for id in result %}
				{% if m.rsc[id].is_visible %}
				<li id="{{ #li.id }}" class="clearfix {% if not m.rsc[id].is_published %} unpublished{% endif %}">
					<a href="{% url admin_edit_rsc id=id %}" class="row">
							<span class="zp-30">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
							<span class="zp-15">{{ m.rsc[m.rsc[id].category_id].title }}</span>
							<span class="zp-15">{{ m.rsc[id].created|date:"d M Y, H:i" }}</span>
							<span class="zp-15">{{ m.rsc[id].modified|date:"d M Y, H:i" }}</span>
							<span class="zp-25">{{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}</span>
                    </a>
                    <span class="button-area">
                        <a href="{{ m.rsc[id].page_url }}" class="button">{_ view _}</a>
                        <a href="{% url admin_edit_rsc id=id %}" class="button">{_ edit _}</a>
                    </span>
				</li>
				{% endif %}
			{% empty %}
				<li>
					{_ No pages found. _}
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_overview_rsc" qargs %}

		{% endwith %}
		
		</div>
	</div>


{% endblock %}
