{% with m.log[id] as l %}
<li id="{{ #li.id }}" style="height: 27px">
        <span class="zp-5">{{ l.type|default:"-" }}</span>
        <span class="zp-25">{{ l.module|default:"-" }}{% if l.line %}:{{ l.line }}{% endif %}</span>
        <span class="zp-45">{{ l.message }}</span>
        <span class="zp-15">
            {% if l.user_id %}
            <a href="{% url admin_edit_rsc id=l.user_id %}">{{ m.rsc[l.user_id].title }}</a>
            {% else %}
            -
            {% endif %}
        </span>
        <span class="zp-10">{{ l.created|date:"d M Y, H:i" }}</span>
</li>
{% endwith %}
