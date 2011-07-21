{% if m.acl.use.mod_menu and m.rsc.main_menu %}
<li><a href="{% url admin_edit_rsc id=m.rsc.main_menu.id %}" {% if id == m.rsc.main_menu.id %}class="current"{% endif %}>{_ Menu _}</a></li>
{% endif %}
