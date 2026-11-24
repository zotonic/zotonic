{% with m.rsc[q["in"]].id as container_id %}
    {% if container_id.is_visible %}
        <nav aria-label="{_ Translation navigation _}">
            <p>
                {_ In _}:
                <a href="{% url admin_translation_texts id=container_id qargs=%{"in": container_id} close=q.close %}">{{ container_id.title|default:_"Untitled" }}</a>
            </p>
            <p>
                {% with id|prev_in:container_id, id|next_in:container_id as prev_id, next_id %}
                    {% if prev_id %}
                        <a class="btn btn-default" rel="prev" href="{% url admin_translation_texts id=prev_id qargs=%{"in": container_id} close=q.close %}">
                            &larr; {_ Previous _}: {{ prev_id.title|default:_"Untitled"|truncatechars:40 }}
                        </a>
                    {% endif %}
                    {% if next_id %}
                        <a class="btn btn-default" rel="next" href="{% url admin_translation_texts id=next_id qargs=%{"in": container_id} close=q.close %}">
                            {_ Next _}: {{ next_id.title|default:_"Untitled"|truncatechars:40 }} &rarr;
                        </a>
                    {% endif %}
                {% endwith %}
            </p>
        </nav>
    {% endif %}
{% endwith %}
