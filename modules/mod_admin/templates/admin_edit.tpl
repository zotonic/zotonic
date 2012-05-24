{% extends "admin_base.tpl" %}

{% block title %}{_ Edit _} “{{ m.rsc[id].title }}”{% endblock %}


{% block content %}
{% with m.rsc[id] as r %}
{% with r.is_editable as is_editable %}
{% with m.config.i18n.language_list.list as languages %}

<div class="edit-header">
	<div class="pull-right span4">
		<p class="admin-chapeau">
		{_ Modified _} {{ r.modified|timesince }}
		{_ by _} <nobr><a href="{% url admin_edit_rsc id=r.modifier_id %}">{{ m.rsc[r.modifier_id].title }}</a>.</nobr>
		{_ Created by _}
			<nobr><a href="{% url admin_edit_rsc id=r.creator_id %}">{{ m.rsc[r.creator_id].title }}</a>.</nobr>
		</p>
	</div>

	{% if not is_editable %}
	<h2>
	{_ You are not allowed to edit the _} {{ m.rsc[r.category_id].title|lower }} “<span {% include "_language_attrs.tpl" %}>{{ r.title|striptags }}</span>”
	</h2>
	{% else %}
	<p class="admin-chapeau">
		{_ editing _}
		{% if m.acl.insert[r.category.name|as_atom] and not r.is_a.meta %}
		<a	href="javascript:;" id="changecategory" title="{_ Change category _}">{{ m.rsc[r.category_id].title|lower }}</a>:
		{% wire id="changecategory" action={dialog_open title=_"Change category" template="_action_dialog_change_category.tpl" id=id} %}
		{% else %}
		{{ m.rsc[r.category_id].title|lower }}:
		{% endif %}
	</p>
	
	<h2 {% include "_language_attrs.tpl" %}>
		{{ r.title|striptags|default:_"<em>untitled</em>" }}
	</h2>
	{% endif %}{# editable #}
</div>

{% block admin_edit_form_pre %}{% endblock %}

{% wire id="rscform" type="submit" postback="rscform" %}
<form id="rscform" method="post" action="postback" class="row">
	<button style="display:none" type="submit"></button><!-- for saving on press enter -->
	<input type="hidden" name="id" value="{{ id }}" />

	<div class="span8" id="poststuff">
		{% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}
        {% include "_admin_edit_content_address.tpl" %}
		{% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %}

		{% if r.is_a.media or r.medium %}
			{% include "_admin_edit_content_media.tpl" %}
		{% endif %}

		{% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}
		{% catinclude "_admin_edit_blocks.tpl" id is_editable=is_editable languages=languages %}
		{% catinclude "_admin_edit_depiction.tpl" id is_editable=is_editable languages=languages %}

{#
		{% catinclude "_admin_edit_haspart.tpl" id is_editable=is_editable languages=languages %}
#}

		{% include "_admin_edit_content_advanced.tpl" %}
		{% include "_admin_edit_content_seo.tpl" %}
	</div>

	<div class="span4" id="sidebar">
		<div id="sort"> {# also sidebar #}

		{# Publish page #}
		{% include "_admin_edit_content_publish.tpl" headline="simple" %}

		{# Access control #}
		{% include "_admin_edit_content_acl.tpl" %}

		{% if not r.is_a.meta %}
				{# Publication period #}
		{% include "_admin_edit_content_pub_period.tpl" %}

		{# Date range #}
		{% include "_admin_edit_content_date_range.tpl" %}
		{% endif %} {# not r.is_a.meta #}

		{% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}

		{# Page connections #}
		{% include "_admin_edit_content_page_connections.tpl" %}
		</div>
	</div>
</form>

{% block admin_edit_form_post %}{% endblock %}

</div>

<script>
	$(function() {
		setTimeout(function() {
		$({{ m.session['admin_widgets']|to_json }}).each(function() {
			for (var k in this) {
				$("#"+k).adminwidget("setVisible", this[k] == "true", true);
			}});
		}, 1);
		
		$('.language-tabs > li > a[data-toggle="tab"]').live('shown', function (e) {
			if (e.target != e.relatedTarget) {
				var lang = $(e.target).parent().attr('lang');
				$("li[lang='"+lang+"']:visible > a").tab('show');
			}
		});
	});
</script>


{% endwith %}
{% endwith %}
{% endwith %}

{% endblock %}
