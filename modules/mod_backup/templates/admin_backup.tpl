{% extends "admin_base.tpl" %}

{% block title %} Admin Backups {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Backups</h2>

			<p>At any moment you can make a backup of your system. The backup comprises two parts, the database and the uploaded files.<br/> 
				You can have 10 backups, older ones will be deleted automatically.</p>
			
			{% if m.acl.is_admin %}
			<p>
				You can start a backup immediately, whilst the backup is running you can continue working.
				 {% button text="Start backup now" action={backup_start} %}
			</p>
			{% endif %}
			
			<div class="zp-50 last">
				<h3 class="above-list clearfix">Backups</h3>
				<ul class="short-list">
					<li class="headers clearfix">
						<span class="zp-40">Date</span>
						<span class="zp-60 last">Actions</span>
					</li>

				{% for id,date,in_progress in backups %}
					<li id="{{ #li.id }}">
						<a href="" class="clearfix">
							<span class="zp-40">
								{{ date|date:"M d Y, H:i" }}
							</span>
							<span class="zp-60 last">
								{% if in_progress %}
									<span class="notice">this backup is in progress</span>
								{% else %}
									{% button text="download database" action={redirect dispatch="backup_download" star=[id, ".sql"]} %}
									{% button text="download files" action={redirect dispatch="backup_download" star=[id, ".tar.gz"]} %}
								{% endif %}
							</span>
						</a>
					</li>
				{% empty %}
					<li>
						No backups present.
					</li>
				{% endfor %}
				</ul>
			</div>

		</div>
	</div>
{% endwith %}
{% endblock %}