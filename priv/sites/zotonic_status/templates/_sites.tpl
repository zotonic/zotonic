<thead>
    <tr>
	<th>{_ Site _}</th>
	<th>{_ Status _}</th>
	{% if has_user %}
	<th>{_ Action _}</th>
	{% endif %}
    </tr>
</thead>

<tbody>

    {% for name,status,childspec,pid,time in sites %}

    {% if name /= 'zotonic_status' and (has_user or not configs[name].hidden) %}
    <tr id="site-{{ name }}" class="{{ status }}">
	<td>
	    <a href="http://{{ configs[name].hostname|escape }}/">http://{{ configs[name].hostname|escape }}/</a>
	</td>
	<td>
	    <span class="waiting status">{_ Waiting _}</span>
	    <span class="running status">{_ Running _}</span>
	    <span class="retrying status">{_ Retrying _}</span>
	    <span class="failed status">{_ Failed _}</span>
	    <span class="stopped status">{_ Stopped _}</span>
	</td>
	{% if has_user %}
	<td>
	    {% button 
	    text=_"start"
	    class="start btn btn-mini" 
	    title=_"Start the site." 
	    postback={site_start site=name} %}

	    {% button 
	    text=_"stop"
	    class="stop btn btn-mini" 
	    title=_"Stop the site." 
	    postback={site_stop site=name} %}

	    {% button 
	    text=_"restart"
	    class="restart btn btn-mini" 
	    title=_"Restart the site, all users will be logged off." 
	    postback={site_restart site=name} %}

	    {% button 
	    text=_"flush"
	    class="flush btn btn-mini" 
	    title=_"Flush and reload all settings, templates etc." 
	    postback={site_flush site=name} %}
	    
	    {% all include "_z_status_button.tpl" %}
	</td>
	{% endif %}
    </tr>
    {% endif %}
    {% endfor %}
</tbody>
