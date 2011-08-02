{% extends "admin_log_base.tpl" %}

{% block title %}{_ Log messages _}{% endblock %}

{% block title_log %}{_ Log messages _}{% endblock %}

{% block content_log %}

		<h3 class="above-list ">
			{_ Most recent messages _}
		</h3>

		{% with m.search[{log page=q.page pagelen=20}] as result %}
			<ul class="short-list">
				<li class="headers clearfix" id="log-headers">
					<span class="zp-5">{_ Type _}</span>
					<span class="zp-25">{_ Module _}</span>
					<span class="zp-45">{_ Message _}</span>
					<span class="zp-15">{_ User _}</span>
					<span class="zp-10">{_ Date _}</span>
				</li>
			</ul>

		   <div class="wrapper" style="height: 620px; overflow: auto">
			   <ul class="short-list" id="log-area">
				{% for id in result %}
					{% include "_admin_log_row.tpl" id=id %}
				{% empty %}
					<li>
						{_ No log messages. _}
					</li>
				{% endfor %}
				</ul>
			   {% button text="more..." action={moreresults result=result target="log-area" template="_admin_log_row.tpl"} %}
		   </div>
		{% endwith %}

        {% wire action={connect signal={log_message} action={addlog target="log-area"}} %}

{% endblock %}
