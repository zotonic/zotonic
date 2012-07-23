{% extends "base.tpl" %}

{# Simple page for TEXT (no media, as simple as possible) #}

{% block content %}
<div {% include "_language_attrs.tpl" id=id %}>
	{% include "_title.tpl" %}
	{% include "_meta.tpl" %}

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

	{% include "_address.tpl" %}

	{% block subnav %}
		{% include "_subnav.tpl" %}
	{% endblock %}

	{% block body %}
	<div class="body">
		{{ m.rsc[id].body }}
		{% include "_blocks.tpl" %}
	</div>
	{% endblock %}

	{% block below_body %}{% endblock %}

	{% block related %}
		{% include "_content_list.tpl" list=id.o.hasfeatured %}
		{% include "_content_list.tpl" list=id.o.hasdocument title=_"Documents"%}
		{% include "_content_list.tpl" list=id.o.depiction title=_"Media"%}

		{% include "_content_list.tpl" list=id.o.haspart title=_"More" %}

		{% include "_content_list.tpl" list=m.rsc.sidebar_collection.o.haspart %}
	{% endblock %}
</div>
{% endblock %}
