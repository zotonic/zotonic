{% extends "admin_base.tpl" %}

{% block title %}{_ Referrers to _} {{ m.rsc[q.id].title }}{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Referrers to _} “{{ m.rsc[q.id].title }}”</h2>
			
		{% with m.search.paged[{referrers id=q.id page=q.page}] as result %}

			{% ifequal result.total 0 %}
				<p>{_ There are no pages with a connection to the page _} “{{ m.rsc[q.id].title }}”</p>
			{% else %}
				<p>{_ The following _} {% ifequal result.total 1 %}{_ page has _}{% else %}{{ result.total }} {_ pages have _}{% endifequal %} {_ a connection to the page _} “{{ m.rsc[q.id].title }}”.</p>
			{% endifequal %}

			{% pager result=result dispatch="admin_referrers" id=q.id qargs %}
			
			<h3 class="above-list">{_ Referrers _}</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-30">{_ Title _}</span>
					<span class="zp-15">{_ Predicate _}</span>
					<span class="zp-15">{_ Category _}</span>
					<span class="zp-15">{_ Modified on _}</span>
					<span class="zp-25">{_ Modified by _}</span>
				</li>
			{% for id, pred_id in result %}
				<li id="{{ #li.id }}" class="clearfix {% if not m.rsc[id].is_published %}unpublished{% endif %}">
					<a href="{% url admin_edit_rsc id=id %}">
						<span class="zp-30">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
						<span class="zp-15">{{ m.rsc[pred_id].title }}</span>
						<span class="zp-15">{{ m.rsc[m.rsc[id].category_id].title }}</span>
						<span class="zp-15">{{ m.rsc[id].modified|date:"d M, H:i" }}</span>
						<span class="zp-25">{{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}</span>
					</a>
                    <span class="button-area">
                        {% button text=_"delete" disabled=m.rsc[id].is_protected action={dialog_delete_rsc id=id on_success={slide_fade_out target=#li.id}} %}
                        {% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
                    </span>
				</li>
			{% empty %}
				<li>
					{_ No referrers found. _}
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_referrers" id=q.id qargs %}

		{% endwith %}

		</div>
	</div>
{% endblock %}
