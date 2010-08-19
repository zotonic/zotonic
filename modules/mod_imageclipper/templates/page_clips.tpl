{% extends "base.tpl" %}

{% block title %}{_ Image clipper _}{% endblock %}

{% block content %}
<h1>{_ My Image clippings _}</h1>

{% with m.session.new_imageclipper_items as new %}
<style type="text/css">
    ul.clippings img { padding: 4px; border: 1px solid #e0e0e0; margin: 0px; }
    ul.clippings img.new { background-color: #ffff99; }
    p.new { background-color: #ffff99; padding: 5px; border: 1px solid #e0e0e0; }
</style>

{% if new %}
<p class="new">{_ The clippings were successfully added to your profile! _}</p>
{% endif %}

<p>
    {_ You have the following image clippings: _}
</p>

<ul class="clippings">
    {% for id in m.search[{query cat="clipping" hassubject=[m.acl.user, 'author'] sort="-rsc.modified"}] %}
    {% if id|member:new %}
    {% image m.rsc[id].medium width=100 height=100 extent class="new" %}
    {% else %}
    {% image m.rsc[id].medium width=100 height=100 extent %}
    {% endif %}
    {% endfor %}

</ul>
{% endwith %}

{% endblock %}
