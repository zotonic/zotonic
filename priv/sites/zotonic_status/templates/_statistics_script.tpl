
{% for node, h, resp in stats %}
{% for k, stats in resp %}

$("#stats-{{ k|statskey }}").html("{% include "_statsline.tpl" k=k stats=stats %}");

p = window.graphs["{{ k | statskey }}"];
p.setData([{{stats.second|statsarray}}]);
p.setupGrid();
p.draw();
{% endfor %}
{% endfor %}

