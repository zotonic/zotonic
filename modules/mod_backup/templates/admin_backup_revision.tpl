{% extends "admin_base.tpl" %}

{% block title %}{_ Revisions for _} {{ id.title }}{% endblock %}

{% block head_extra %}
	{% lib "js/wdiff.js" "js/modules/z.make_diff.js" %}

	<style type="text/css">
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

<div class="admin-header">

	{% if id.exists %}
        <h2>{_ Revisions for _} {{ id.title }}</h2>
    {% else %}
        <h2>{_ Revisions for _} <em>{_ Deleted _}</em></h2>
    {% endif %}

    <p>
    	{_ Check and possibly restore an earlier version of your page. _}
    	{% if id.exists %}
    		<a href="{% url admin_edit_rsc id=id %}">{_ Back to the edit page _}</a>
    	{% endif %}
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
		{% for rev in m.backup_revision.list[id] %}
			{% if forloop.first %}
			<li>
				<h4>{_ Previous _}</h4>
			</li>
			{% endif %}
			<li>
				<label class="rev-a"><input type="radio" name="a" value="{{ rev.id }}" /></label>
				<label class="rev-b"><input type="radio" name="b" value="{{ rev.id }}" style="visibility: hidden"/></label>
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
	a: "latest", 
	z_delegate: `controller_admin_backup_revision`
});
{% endjavascript %}

{% endblock %}

