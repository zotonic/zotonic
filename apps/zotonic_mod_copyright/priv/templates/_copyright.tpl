{% with m.copyright.lookup[id.rights|default:m.copyright.rights] as rights %}
{% with id.rights_attribution|default:m.copyright.attribution as attribution %}
{% with id.rights_year|default:m.copyright.year as year %}
<div class="copyright">
    {{ rights.prefix }}
    {% block copyright_icon %}
        {% for icon in rights.icons %}
            <img src="{{ icon }}" style="height:1em;width:auto;display:inline-block;margin:0;padding:0;vertical-align:bottom">
        {% endfor %}
    {% endblock %}
    {{ year }} {% if rights.url %}<a href="{% right.url %}" target="_blank" rel="noopener noreferrer">{{ rights.title }}</a>{% else %}{{ rights.title }}{% endif %}{% if attribution %} &ndash; {{ attribution }}{% endif %}
</div>
{% endwith %}
{% endwith %}
{% endwith %}
