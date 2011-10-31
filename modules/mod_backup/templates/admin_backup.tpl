{% extends "admin_base.tpl" %}

{% block title %} {_ Admin Backups _} {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

            <div class="clearfix">

			<h2>{_ Backups _}</h2>

			<p>{_ At any moment you can make a backup of your system. _} {_ The backup comprises two parts, the database and the uploaded files. _}<br/> 
				{_ You can have 10 backups, older ones will be deleted automatically. _}</p>
			
            {% if backup_config.ok %}
			{% if m.acl.is_admin %}
			<p>
				{_ You can start a backup immediately, whilst the backup is running you can continue working. _}
				 {% button text=_"Start backup now" action={backup_start} %}
			</p>
			{% endif %}
            {% else %}
            <p class="error">
                <strong>{_ Warning: _}</strong> {_ Your backup is not correctly configured. The backup module will not work until the problem(s) below have been resolved: _}
                {% if not backup_config.db_dump %}<br/><strong>{_ The "pg_dump" command was not found in the path. Set the "mod_backup.pg_dump" config key to the path to pg_dump and return to this page. _}</strong>{% endif %}
                {% if not backup_config.archive %}<br/><strong>{_ The "tar" command was not found in the path. Set the "mod_backup.tar" config key to the path to pg_dump and return to this page. _}</strong>{% endif %}
            </p>
            {% endif %}

            </div>
			<div class="zp-50 last">
				<h3 class="above-list clearfix">{_ Backups _}</h3>
				<ul class="short-list">
					<li class="headers clearfix">
						<span class="zp-60">{_ Date _}</span>
						<span class="zp-40 last">{_ Actions _}</span>
					</li>

				{% for id,date,in_progress in backups %}
					<li id="{{ #li.id }}" class="clearfix">
						<a href="" >
							<span class="zp-100">
								{{ date|date:"M d Y, H:i" }}
							</span>
                        </a>
                        <span class="button-area">
                            {% if in_progress %}
                            <span class="notice">{_ this backup is in progress _}</span>
                            {% else %}
                            <a class="button" href="{% url backup_download star=[id, ".sql"] %}">{_ download database _}</a>
                            <a class="button" href="{% url backup_download star=[id, ".tar.gz"] %}">{_ download files _}</a>
                            {% endif %}
                        </span>
					</li>
				{% empty %}
					<li>
						{_ No backups present. _}
					</li>
				{% endfor %}
				</ul>
			</div>

		</div>
	</div>
{% endwith %}
{% endblock %}
