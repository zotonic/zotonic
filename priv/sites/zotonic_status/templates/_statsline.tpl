{% for n in nodes %}
<span title="min: {{ stats[n][type].min|statsvalue:type }}, max: {{ stats[n][type].max|statsvalue:type }}"><strong>{{ n }}</strong>: 99% &lt; <strong>{{ stats[n][type].perc99|statsvalue:type }}</strong>, 95% &lt; <strong>{{ stats[n][type].perc95|statsvalue:type }}</strong>, avg: <strong>{{ stats[n][type].avg|statsvalue:type }}</strong></span><br/>
{% endfor %}
