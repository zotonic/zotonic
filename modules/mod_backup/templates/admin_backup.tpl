{% extends "admin_base.tpl" %}

{% block title %} {_ Admin Backups _} {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Backups _}</h2>

			<p>{_ At any moment you can make a backup of your system. _} {_ The backup comprises two parts, the database and the uploaded files. _}<br/> 
				{_ You can have 10 backups, older ones will be deleted automatically. _}</p>
			
			{% if m.acl.is_admin %}
			<p>
				{_ You can start a backup immediately, whilst the backup is running you can continue working. _}
				 {% button text=_"Start backup now" action={backup_start} %}
			</p>
			{% endif %}
			
			<div class="zp-50 last">
				<h3 class="above-list clearfix">{_ Backups _}</h3>
				<ul class="short-list">
					<li class="headers clearfix">
						<span class="zp-40">{_ Date _}</span>
						<span class="zp-60 last">{_ Actions _}</span>
					</li>

				{% for id,date,in_progress in backups %}
					<li id="{{ #li.id }}">
						<a href="" class="clearfix">
							<span class="zp-40">
								{{ date|date:"M d Y, H:i" }}
							</span>
							<span class="zp-60 last">
								{% if in_progress %}
									<span class="notice">{_ this backup is in progress _}</span>
								{% else %}
									{% button text=_"download database" action={redirect dispatch="backup_download" star=[id, ".sql"]} %}
									{% button text=_"download files" action={redirect dispatch="backup_download" star=[id, ".tar.gz"]} %}
								{% endif %}
							</span>
						</a>
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