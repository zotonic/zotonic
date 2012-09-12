{% extends "admin_base.tpl" %}

{% block title %}{_ Revisions for _} {{ id.title }}{% endblock %}

{% block head_extra %}
	{% lib "js/wdiff.js" "js/modules/z.make_diff.js" %}

	<style type="text/css">
		#revisions .active a.first {
			background-color: green;
		}
		#revisions .active a.second {
			background-color: red;
		}
		.diff-omitted {
			color: #999;
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

<div class="edit-header">

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

	<div class="span3">
		<ul class="nav nav-list" id="revisions">
		{% if id.exists %}
			<li class="nav-header">
				{_ Current _}
			</li>
			<li class="active">
				<a href="#={{ id }}" class="first">
					{{ id.modified|date:"Y-m-d H:i" }}
					{{ id.modifier_id.title }}
				</a>
			</li>
		{% endif %}
		{% for rev in m.backup_revision.list[id] %}
			{% if forloop.first %}
			<li class="nav-header">
				{_ Previous _}
			</li>
			{% endif %}
			<li>
				<a href="#{{rev.id}}">
					{{ rev.created|date:"Y-m-d H:i" }}
					{{ rev.user_name }}
				</a>
			</li>
		{% endfor %}
		</ul>
	</div>

	<div class="span9">
		<div id="page-diff" >
			<p class="alert">
				{_ Select one or two revisions from the list on the left. _}
			</p>
		</div
	</div>

</div>

{% javascript %}
var $last_clicked;

$('#revisions a').click(function(event) {
	$(this).closest('li').toggleClass('active');
	$('#revisions li a').removeClass('first second');

	var checked = $('#revisions li.active a');

	if (checked.length > 2) {
		$last_clicked.closest('li').removeClass('active');
		checked = $('#revisions li.active a');
	}
	$last_clicked = $(this);

	if (checked.length == 2) {
		$(checked.get(0)).addClass("first");
		$(checked.get(1)).addClass("second");

		z_notify("rev-diff", { 
			a: $(checked.get(0)).attr('href'), 
			b: $(checked.get(1)).attr('href'), 
			z_delegate: 'resource_admin_backup_revision' 
		});
	} else if (checked.length == 1) {
		$(checked.get(0)).addClass("first");

		z_notify("rev-diff", { 
			a: $(checked.get(0)).attr('href'), 
			z_delegate: 'resource_admin_backup_revision' 
		});
	}
	event.preventDefault();
});
{% endjavascript %}

{% endblock %}

