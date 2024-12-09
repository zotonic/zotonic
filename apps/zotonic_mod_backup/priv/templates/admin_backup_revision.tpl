{% extends "admin_base.tpl" %}

{% block title %}{_ Revisions for _} {{ id.title }}{% endblock %}

{% block head_extra %}
	<style type="text/css" nonce="{{ m.req.csp_nonce }}">
		#revisions label {
			display: inline-block;
			height: 20px;
			width: 20px;
			text-align: center;
			margin: 0 0 1px 0;
			padding: 0;
		}
		.diff-omitted {
			color: #999;
			color: white;
		}
		ins {
			color: green;
		}
		del {
			color: red;
		}
	</style>
{% endblock %}


{% block content %}

{% if id.exists and not id.is_editable %}
     <div class="alert alert-warning" role="alert">
        <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>
        {_ You are not allowed to edit this page. _}
    </div>
{% elseif not id.exists and not m.acl.use.mod_backup %}
     <div class="alert alert-warning" role="alert">
        <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>
        {_ You are not allowed to recover this page. _}
    </div>
{% else %}
	{% with not id.exists as is_deleted %}
	{% with m.backup_revision.list[id] as revs %}
		<div class="admin-header">
			{% if id.exists %}
		        <h2>{_ Revisions for _} {{ id.title }}</h2>
		    {% else %}
		        <h2>{_ Revisions for _} <em>{_ Deleted _}</em></h2>
		    {% endif %}

	    	{% if id.exists %}
			    <p>
		    		<a class="btn btn-default" href="{% url admin_edit_rsc id=id %}">{_ Back to the edit page _}</a>
			    </p>
	    	{% endif %}
		    <p>
		    	{_ Check and possibly restore an earlier version of your page. _}<br>
		    	{% trans "Revisions are kept for {n} months." n=m.backup_revision.retention_months %}<br>
				{% trans "Revisions of active users are kept for {n} days." n=m.backup_revision.user_retention_days %}<br>
				{% trans "Revisions of deleted users are kept for {n} days." n=m.backup_revision.deleted_user_retention_days %}
		    </p>
		</div>

		<div class="row">

			<div class="col-lg-3 col-md-3">
				<ul class="nav nav-list" id="revisions">
				{% if id.exists %}
					<li class="active">
						<label class="rev-a"><input type="radio" name="a" value="latest" checked /></label>
						<label class="rev-b"><input type="radio" name="b" value="latest" checked /></label>
						<span>
							{{ id.modified|date:"Y-m-d H:i" }}
							{{ id.modifier_id.title }}
						</span>
					</li>
				{% endif %}
				{% for rev in revs %}
					{% if forloop.first %}
					<li>
						<h4>{_ Previous _}</h4>
					</li>
					{% endif %}
					<li>
						<label class="rev-a">
							<input type="radio" name="a" value="{{ rev.id }}" {% if forloop.first and is_deleted %} checked{% endif %}>
						</label>
						<label class="rev-b">
							<input type="radio" name="b" value="{{ rev.id }}" {% if forloop.first and is_deleted %} checked{% else %}style="visibility: hidden"{% endif %}>
						</label>
						<span>
							{{ rev.created|date:_"Y-m-d H:i" }}
							{{ rev.user_id.title|default:rev.user_name }}
						</span>
					</li>
				{% endfor %}
				</ul>
			</div>

			<div class="col-lg-9 col-md-9">
				<div id="page-diff" >
				</div
			</div>

		</div>

		{% javascript %}
		$('#revisions input').change(function(event) {
			var a = $('#revisions input[name="a"]:checked').val();
			var b = $('#revisions input[name="b"]:checked').val();
			var is_show = false;
			$('#revisions input[name="a"]').each(function() {
				if ($(this).val() == b) {
					is_show = true;
				}
				if (is_show) {
					$(this).css({visibility: "visible"});
				} else {
					$(this).css({visibility: "hidden"});
				}
			});
			is_show = true;
			$('#revisions input[name="b"]').each(function() {
				if (is_show) {
					$(this).css({visibility: "visible"});
				} else {
					$(this).css({visibility: "hidden"});
				}
				if ($(this).val() == a) {
					is_show = false;
				}
			});

			if (a == b) {
				z_notify("rev-diff", {
					id: {{ id }},
					a: a,
					z_delegate: `controller_admin_backup_revision`
				});
			} else {
				z_notify("rev-diff", {
					id: {{ id  }},
					a: a,
					b: b,
					z_delegate: `controller_admin_backup_revision`
				});
			}
		});

		z_notify("rev-diff", {
			id: {{ id }},
			a: $('#revisions input[name="a"]:checked').val(),
			z_delegate: `controller_admin_backup_revision`
		});
		{% endjavascript %}
	{% endwith %}
	{% endwith %}
{% endif %}

{% endblock %}

{% block js_extra %}
	{% lib "js/wdiff.js" "js/modules/z.make_diff.js" %}
{% endblock %}


