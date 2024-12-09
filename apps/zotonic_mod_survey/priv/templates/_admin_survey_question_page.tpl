<li class="page" id="{{ #p }}">
	<div class="options btn-group pull-right">
		<a class="btn btn-default btn-sm dropdown-toggle" data-toggle="dropdown" href="#">
			<span class="caret"></span>
		</a>
		<ul class="dropdown-menu" role="menu" aria-labelledby="dLabel">
			<li><a tabindex="-1" href="#question-prepend">{_ Add question _}</a></li>
			<li class="divider"></li>
			<li><a tabindex="-1" href="#page-above">{_ Add page above _}</a></li>
			<li><a tabindex="-1" href="#page-below">{_ Add page below _}</a></li>
			<li class="divider"></li>
			<li><a tabindex="-1" href="#page-delete">{_ Delete page _}</a></li>
			<li class="divider"></li>
			<li><a tabindex="-1" href="#outline-toggle">{_ Toggle outline view _}</a></li>
		</ul>
	</div>

	<h3 class="page-header">&nbsp;</h3>

	<div class="questions">
		<ul class="question-list">
			{% for blk in qs %}
				{% include "_admin_survey_question_q.tpl" %}
			{% endfor %}
		</ul>
		<ul class="nav nav-pills">
			<li><a href="#question-append">{_ Add question _}</a></li>
		</ul>
	</div>

	{% with js|survey_is_stop as is_stop %}
	<div class="jumps">
		<div class="form-group" style="padding-left: 15px">
			<label class="checkbox">
				<input type="checkbox" class="is_stop_page {% if nosubmit %}nosubmit{% endif %}" name="is_stop_page" {% if is_stop %}checked{% endif %} />
				{_ Remove "Next" button. No answers will be submitted unless you add a button jump to a next page. _}
			</label>
		</div>

		<ul class="jump-list">
			{% for blk in js %}
				{% if blk.target1 or blk.condition1 %}
					{% include "_admin_survey_question_j.tpl" target=blk.target1 condition=blk.condition1 %}
				{% endif %}
				{% if blk.target2 or blk.condition2 %}
					{% include "_admin_survey_question_j.tpl" target=blk.target2 condition=blk.condition2 %}
				{% endif %}
			{% endfor %}
		</ul>
		<ul class="nav nav-pills">
			<li><a href="#jump-add">{_ Add page jump _}</a></li>
		</ul>
	</div>
	{% endwith %}
</li>
