{% for type in types %}
p = window.graphs["{{ type | statskey }}"];
p.setData({{ stats|statsarray : type : 'second' }});
p.setupGrid();
p.draw();
{% endfor %}

{# FIXME - work out html escaping to prevent JS error
   $("#stats-{{ type|statskey }}").html("{% include "_statsline.tpl" type=type stats=stats %}"); #}
