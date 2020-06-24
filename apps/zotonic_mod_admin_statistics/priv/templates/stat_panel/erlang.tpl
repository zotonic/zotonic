{% extends "_stats_panel.tpl" %}


{% block panel_title %}{_ Erlang VM _}{% endblock %}


{% block panel_body %}
<table class="table table-condensed">
    <thead></thead>
    <tbody>
        {% for title, id in [
                ["System", ""],
                ["Run Queue", "run-queue"],
                ["IO", ""],
                ["Input", "io-in"],
                ["Output", "io-out"],
                ["Garbage Collection", ""],
                ["GC", "gc-r"] ] %}
            {% include "_stat_row.tpl" %}
        {% endfor %}
    </tbody>
</table>


{% endblock %}
