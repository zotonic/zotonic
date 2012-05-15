{% if list %}
{% if title %}<h3>{{ title }}</h3>{% endif %}
<ul class="unstyled content-list list-partof {{ class }}">
{% for pid in list %}
<li>
    <h3><a href="{{ pid.page_url }}">{{ pid.title|default:"&mdash;" }}</a></h3>
    {% if id.summary %}<p>{{id.summary|truncate:60}} <a href="{{ id.page_url }}">{_ more… _}</a></p>
    {% elseif id.body %}<p>{{ id.body|striptags|truncate:60 }} <a href="{{ id.page_url }}">{_ more… _}</a></p>
    {% endif %}
    {% with pid.o.haspart as ps %}
        {% for p in ps %}
        {% if p == id %}
            {% with ps[forloop.counter-1], ps[forloop.counter+1] as prev, next %}
            <ul class="pager">
              {% if prev %}
              <li class="previous">
                <a href="{{ prev.page_url }}">&larr;</a>
              </li>
              {% endif %}
              {% if next %}
              <li class="next">
                <a href="{{ next.page_url }}">&rarr;</a>
              </li>
              {% endif %}
            </ul>
            {% endwith %}
        {% endif %}
        {% endfor %}
    {% endwith %}
</li>
{% endfor %}
</ul>
{% endif %}
