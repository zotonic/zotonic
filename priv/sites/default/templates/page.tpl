{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block content %}

    <h1>{{ m.rsc[id].title }}</h1>

    <p class="summary">
        {{ m.rsc[id].summary }}
    </p>

    {% if m.rsc[id].medium %}
    
    	<figure class="image-wrapper block-level-image">
			{% media m.rsc[id].medium width=445 crop class=align alt=m.rsc[id].title %}
		</figure>
    
    {% endif %}

    {{ m.rsc[id].body|show_media }}

	{% block below_body %}{% endblock %}

{% endblock %}
