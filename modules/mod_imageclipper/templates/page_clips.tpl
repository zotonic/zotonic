{% extends "base.tpl" %}

{% block title %}{_ Image clipper _}{% endblock %}

{% block content %}
<h1>{_ My Image clippings _}</h1>

{% with m.session.new_imageclipper_items as new %}
<style type="text/css">
    #clippings img { padding: 4px; border: 1px solid #e0e0e0; margin: 0px; }
    #clippings img.new { background-color: #ffff99; }
    p.new { background-color: #ffff99; padding: 5px; border: 1px solid #e0e0e0; }
</style>

{% if new %}
<p class="new">{_ The clippings were successfully added to your profile! _}</p>
{% endif %}

<p>
    {_ You have the following image clippings: _}
</p>

{% with m.search[{query cat="clipping" hasobject=[m.acl.user, 'author'] sort="-rsc.modified" pagelen=32}] as result %}
<ul id="clippings">
    {% for id in result %}
    {% include "_clipping.tpl" new=id|member:new %}
    {% endfor %}
</ul>

<div class="clearfix">
    {% button text=_"More" action={moreresults result=result target="clippings" template="_clipping.tpl"} %}
</div>
{% endwith %}
{% endwith %}

{% endblock %}
