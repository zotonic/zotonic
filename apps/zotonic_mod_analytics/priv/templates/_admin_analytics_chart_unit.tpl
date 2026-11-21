{# Counts per displayed graph interval, including chart aggregation. #}
<p class="text-muted">
{% if panel.report.popular %}
    {_ Requests per content item over the selected period _}
{% elseif c.event == "page_request" or c.event == "api_request" or c.event == "request" %}
    {% if panel.report.chart_interval_unit == "minute" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Requests per minute _}
        {% else %}
            {% trans _"Requests per {n} minutes" n=panel.report.chart_interval_count %}
        {% endif %}
    {% elseif panel.report.chart_interval_unit == "hour" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Requests per hour _}
        {% else %}
            {% trans _"Requests per {n} hours" n=panel.report.chart_interval_count %}
        {% endif %}
    {% elseif panel.report.chart_interval_unit == "day" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Requests per day _}
        {% else %}
            {% trans _"Requests per {n} days" n=panel.report.chart_interval_count %}
        {% endif %}
    {% endif %}
{% elseif c.event == "smtp" %}
    {% if panel.report.chart_interval_unit == "minute" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Email events per minute _}
        {% else %}
            {% trans _"Email events per {n} minutes" n=panel.report.chart_interval_count %}
        {% endif %}
    {% elseif panel.report.chart_interval_unit == "hour" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Email events per hour _}
        {% else %}
            {% trans _"Email events per {n} hours" n=panel.report.chart_interval_count %}
        {% endif %}
    {% elseif panel.report.chart_interval_unit == "day" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Email events per day _}
        {% else %}
            {% trans _"Email events per {n} days" n=panel.report.chart_interval_count %}
        {% endif %}
    {% endif %}
{% else %}
    {% if panel.report.chart_interval_unit == "minute" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Events per minute _}
        {% else %}
            {% trans _"Events per {n} minutes" n=panel.report.chart_interval_count %}
        {% endif %}
    {% elseif panel.report.chart_interval_unit == "hour" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Events per hour _}
        {% else %}
            {% trans _"Events per {n} hours" n=panel.report.chart_interval_count %}
        {% endif %}
    {% elseif panel.report.chart_interval_unit == "day" %}
        {% if panel.report.chart_interval_count == 1 %}
            {_ Events per day _}
        {% else %}
            {% trans _"Events per {n} days" n=panel.report.chart_interval_count %}
        {% endif %}
    {% endif %}
{% endif %}
</p>
