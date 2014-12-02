{% if not m.acl.user %}
<p>
   <a href="{% url logon %}">{_ Back to logon form _}</a>
</p>
{% endif %}