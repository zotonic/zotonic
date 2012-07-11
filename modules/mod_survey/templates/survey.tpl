{% extends "page.tpl" %}

{% block main %}

<style type="text/css">
    .survey-narrative input,
    .survey-narrative select {
        vertical-align: baseline;
        width: auto;
    }
</style>

<div id="survey-question">
    {% media m.rsc[id].media[1] width=400 height=400 class="main-image" %}

    <p class="summary">{{ m.rsc[id].summary }}</p>
    {{ m.rsc[id].body|show_media }}

	{% include "_survey_start.tpl" %}
</div>

{% endblock %}
