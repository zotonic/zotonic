{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block content %}

    <h1>{{ m.rsc[id].title }}</h1>

    <p class="summary">
        {{ m.rsc[id].summary }}
    </p>

    {% if m.rsc[id].medium %}
    
    	<figure class="image-wrapper block-level-image">
			{% image id width=445 %}
			<p class="image-caption">This is a lady with a guitar.</p>
		</figure>
    
    {% endif %}

    {{ m.rsc[id].body|show_media }}

	{% block below_body %}{% endblock %}

{% endblock %}