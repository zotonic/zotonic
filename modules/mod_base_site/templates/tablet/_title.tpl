{% with q.in_collection|default:id.s.haspart[1] as in_collection %}
{% if in_collection %}
{% with m.rsc[in_collection].o.haspart as ps %}
    {% for p in ps %}
    {% if p == id %}
        {% with ps[forloop.counter-1], ps[forloop.counter+1] as prev, next %}
        <ul class="pager pull-right">
          {% if prev %}
          <li><a href="{% url page id=prev in_collection=in_collection %}">&larr;</a></li>
          {% endif %}
          {% if next %}
          <li><a href="{% url page id=next in_collection=in_collection %}">&rarr;</a></li>
          {% endif %}
        </ul>
        {% endwith %}
    {% endif %}
    {% endfor %}
{% endwith %}
{% endif %}
{% endwith %}
<h1>{{ m.rsc[id].title }}</h1>
