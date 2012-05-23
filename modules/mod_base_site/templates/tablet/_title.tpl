{% with q.in_collection|default:id.s.haspart[1] as in_collection %}
{% if in_collection %}
{% with m.rsc[in_collection].o.haspart as ps %}
    {% for p in ps %}
    {% if p == id %}
        {% with ps[forloop.counter-1], ps[forloop.counter+1] as prev, next %}
        {#
        <ul class="pager">
          {% if prev %}
          <li><a href="{% url page id=prev in_collection=in_collection %}">&larr;</a></li>
          {% endif %}
          <li><a href="">{{ m.rsc[in_collection].title }}</a></li>
          {% if next %}
          <li><a href="{% url page id=next in_collection=in_collection %}">&rarr;</a></li>
          {% endif %}
        </ul>
        #}
        <ul class="pager-collection">
            {% if prev %}<li class="previous"><a href="{% url page id=prev in_collection=in_collection %}">&larr; {_ Previous _}</a></li>{% endif %}
            {% if next %}<li class="next"><a href="{% url page id=next in_collection=in_collection %}">{_ Next _} &rarr;</a></li>{% endif %}
            <li class="up"><a href="{{ m.rsc[in_collection].page_url }}">{{ m.rsc[in_collection].title }}</a></li>
        </ul>
        {% endwith %}
    {% endif %}
    {% endfor %}
{% endwith %}
{% endif %}
{% endwith %}
<h1>{{ m.rsc[id].title }}</h1>
