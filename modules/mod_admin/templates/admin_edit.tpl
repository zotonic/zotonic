{% extends "admin_base.tpl" %}

{% block title %}{_ Edit _} “{{ m.rsc[id].title }}”{% endblock %}


{% block content %}
{% with m.rsc[id] as r %}
    {% with r.is_editable as is_editable %}
	{% with m.config.i18n.language_list.list as languages %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			{% if not is_editable %}
				<h2>
					{_ You are not allowed to edit the _} {{ m.rsc[r.category_id].title|lower }} “{{ r.title|striptags }}”
				</h2>
			{% else %}
				<p class="admin-chapeau">{_ editing _}:
					<span class="right" style="text-align: right">
						{_ Modified _} {{ r.modified|timesince }}
						{_ by _} {{ m.rsc[r.modifier_id].title }}.<br/>
						{_ Created by _} {{ m.rsc[r.creator_id].title }}.<br/>
					</span>
				</p>
				<h2>{{ r.title|striptags|default:"<em>untitled</em>" }}
					<span>{{ m.rsc[r.category_id].title|lower }} <a href="#category">{_ change _}</a></span>
				</h2>
			{% endif %}

			{% block admin_edit_form_pre %}{% endblock %}

			{% wire id="rscform" type="submit" postback="rscform" %}
			<form id="rscform" method="post" action="postback">
				<input type="hidden" name="id" value="{{ id }}" />

				<div class="zp-67" id="poststuff">
					<div class="padding">

						{% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}
						{% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %}

						{% if r.is_a.media or r.medium %}
							{% include "_admin_edit_content_media.tpl" %}
						
							{% if is_editable %}
								{% include "_admin_edit_content_website.tpl" %}
							{% endif %}
						{% endif %}{# medium #}


						{% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}

						{% catinclude "_admin_edit_depiction.tpl" id is_editable=is_editable languages=languages %}

						{% include "_admin_edit_content_advanced.tpl" %}
						{% include "_admin_edit_content_seo.tpl" %}
					</div>
				</div>

				<div class="zp-33" id="sidebar">
					<div class="padding" id="sort">	{# also sidebar #}

						{# Publish page #}
						{% include "_admin_edit_content_publish.tpl" headline="simple" %}

						{# Access control #}
						{% include "_admin_edit_content_acl.tpl" %}

						{% if not r.is_a.meta %}
							{# Publication period #}
							{% include "_admin_edit_content_pub_period.tpl" %}

							{# Date range #}
							{% include "_admin_edit_content_date_range.tpl" %}
						{% endif %}	{# not r.is_a.meta #}

						{% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}

						{# Page connections #}
						{% include "_admin_edit_content_page_connections.tpl" %}

						{% if m.acl.insert[r.category.name|as_atom] %}
							{% include "_admin_edit_content_category.tpl" %}
						{% endif %}
					</div>
				</div>
			</form>

		{% block admin_edit_form_post %}{% endblock %}

		</div>
	</div>
	{% endwith %}
    {% endwith %}
{% endwith %}
{% endblock %}
