{% extends "_stats_panel.tpl" %}

{% block panel_title %}{_ Erlang VM _}{% endblock %}

{% block panel_body %}
<table class="table table-condensed">
    <thead></thead>
    <tbody>
        {% for title, id in [
                ["System", ""],
                ["Run Queue", "statistics-run_queue"],
                ["Garbage Collection", ""],
                ["Total GCs", "gc-total_coll"],
                ["Words Reclaimed", "gc-rec_wrd"]
            ] %}
            {% include "_stat_row.tpl" %}
        {% endfor %}
    </tbody>
</table>
{% endblock %}
