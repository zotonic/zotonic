{% extends "page.tpl" %}

{% block breadcrumb %}
	{% with m.rsc[id].category_id as category_id %}

		<p class="breadcrumb">
			<a href="{{ m.rsc.page_home.page_url }}">{{ m.rsc.page_home.short_title | default: m.rsc.page_home.title}}</a> &raquo;
			
			{% for cat_id in m.category[category_id].path %}
				{% ifnotequal m.rsc[cat_id].name "text" %}
					<a href="{{ m.rsc[cat_id].page_url }}">{{ m.rsc[cat_id].short_title | default: m.rsc[cat_id].title }}</a> &raquo;
				{% endifnotequal %}
			{% endfor %}

            <a href="{{ m.rsc[category_id].page_url }}">{{ m.rsc[category_id].title }}</a> &raquo;

            <select onchange="document.location.href=$(this).val();">
                {% for c_id in m.search[{query cat=category_id}] %}
                <option value="{{ m.rsc[c_id].page_url }}" {% if c_id == id %}selected="selected"{% endif %}>{{ m.rsc[c_id].short_title | default: m.rsc[c_id].title }}</option>
                {% endfor %}
            </select>
		</p>
	{% endwith %}
{% endblock %}

{% block content_class %}{% if m.rsc[id].o.document%}zp-67{% else %}zp-100{% endif %}{% endblock %}

{% block sidebar %}
{% include "_documents.tpl" %}
{% endblock %}	

{% block below_content %}
<p class="zotonic-license-notice">
    This page is part of the Zotonic documentation, which is licensed under the <a href="http://www.apache.org/licenses/LICENSE-2.0.html">Apache License 2.0</a>.
</p>
{% endblock %}
