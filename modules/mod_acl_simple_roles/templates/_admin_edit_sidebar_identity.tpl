<h3>{_ Member of roles _}</h3>

<ul>
{% for role_id in m.rsc[id].s.acl_role_member %}
	<li><a href="{% url admin_edit_rsc id=role_id %}">{{ m.rsc[role_id].title }}</a></li></li>
{% empty %}
	<li><a href="{% url admin_edit_rsc id=m.rsc.role_member.id %}">{{ m.rsc.role_member.title }}</a> {_ (implicit) _}</li>
{% endfor %}
</ul>

<div class="clear">&nbsp;</div>
