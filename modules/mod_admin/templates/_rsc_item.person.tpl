{% extends "_rsc_item.tpl" %}

{% block title %}
    {% include "_name.tpl" %}
{% endblock %}

{% block meta %}
<div class="text-muted">
    {% catinclude "_admin_overview_list_data.tpl" id %}
</div>
{% endblock %}
