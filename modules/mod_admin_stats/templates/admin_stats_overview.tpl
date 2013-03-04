{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block head_extra %}
    {% lib
        "js/d3.js"
        "js/charts/z_charts.js"
        "js/charts/histogram-duration.js"
        "js/charts/line-chart.js"
        "js/charts/stats_charts.js"
        "css/charts.css"
    %}
{% endblock %}

{% block content %}

    <div class="edit-header">
        <h2>{_ Site Statistics _}</h2>
        {% button text="start"
            action={script script="
                if (!updateTimer) z_notify('update_metrics')"
            }
        %}
        {% button text="stop"
            action={script script="
                clearTimeout(updateTimer); updateTimer = null"
            }
        %}
    </div>

    <div id="test"></div>
    <div id="graphs"></div>

    {% wire name='new_metrics'
        action={script
            script="d3.select('#graphs').call(
            z_charts.update, zEvtArgs, factory);
            updateTimer = setTimeout(
            \"z_notify('update_metrics')\", 2000)"
        }
    %}

    {% wire
        action={script
            script="var factory = stats_chart_factory();
            var updateTimer;
            z_notify('update_metrics');"
        }
    %}

 {% endblock %}
