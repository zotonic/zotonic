{% extends "page.tpl" %}

{% block content %}
    <div {% block content_attributes %}{% include "_language_attrs.tpl" id=id class="wrapper" %}{% endblock %}>
        <h1>{{ m.rsc[id].title }}</h1>
        <div id="survey-question" class="survey-results">
        	{% include "_survey_results.tpl" hide_headers %}
        </div>
    </div>
{% endblock %}
