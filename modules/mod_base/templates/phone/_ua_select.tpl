{# ua selector for non-text devices #}
{% with m.req.ua_class as uac %}
<div class="btn-group {% if dropup %}dropup{% endif %}" id="ua-select">
    <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
        {_ Mobile or desktop _}
        <span class="caret"></span>
     </a>
    <ul class="dropdown-menu">
        <li><a href="{% url ua_select ua_class='desktop' %}">{_ Desktop _} {% if uac == `desktop` %}<i class="icon-ok"></i>{% endif %}</a></li>
        <li><a href="{% url ua_select ua_class='tablet' %}">{_ Tablet _} {% if uac == `tablet` %}<i class="icon-ok"></i>{% endif %}</a></li>
        <li><a href="{% url ua_select ua_class='phone' %}">{_ Phone _} {% if uac == `phone` %}<i class="icon-ok"></i>{% endif %}</a></li>
        <li><a href="{% url ua_select ua_class='text' %}">{_ Text _}</a></li>
        <li><a href="{% url ua_select ua_class='automatic' %}">{_ Automatic _}</a></li>
    </ul>
</div>
{% endwith %}
