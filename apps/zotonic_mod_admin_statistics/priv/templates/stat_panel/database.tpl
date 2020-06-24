{% extends "_stats_panel.tpl" %}

{% block panel_title %}{_ Database _}{% endblock %}

{% block panel_body %}
<div class="row">
    <div class="col-md-6"><span class="meta">Pool Usage: </span><strong>#</strong><span class="meta">%</span></div>
    <div class="col-md-6 text-right">
        <span>#</span>/<span>#</span> 
    </div>
</div>
<div class="progress">
    <div class="progress-bar progress-bar-success" role="progressbar" style="width: 0%">
    </div>
</div>
<table class="table table-condensed">
    <thead></thead>
    <tbody>
        {% for title, id in [
            ["Checkouts/min", "db-checkouts"],
            ["Requests/min", "db-requests"],

                ] %}
        {% include "_stat_row.tpl" %}
        {% endfor %}
    </tbody>
    {% javascript %}
    //$("#statistics-run_queue").data("render", render_value);
    {% endjavascript %}
</table>
{% endblock %}
