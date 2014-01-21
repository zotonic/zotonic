<li class="page" id="{{ #p }}">
	<h3 class="page-header">&nbsp;</h3>
	<ul class="nav nav-pills">
		<li><a href="#page-above">{_ Add page above _}</a></li>
		<li><a href="#page-below">{_ Add page below _}</a></li>
		<li><a href="#page-delete">{_ Delete page _}</a></li>
	</ul>

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
		<div class="control-group">
			<label class="checkbox">
				<input type="checkbox" class="is_stop_page {% if nosubmit %}nosubmit{% endif %}" name="is_stop_page" {% if is_stop %}checked{% endif %} /> 
				{_ Stop the survey after this page. No questions are submitted. _}
			</label>
		</div>

		<ul class="jump-list" {% if is_stop %}style="display:none"{% endif %}>
			{% for blk in js %}
				{% if blk.target1 or blk.condition1 %}
					{% include "_admin_survey_question_j.tpl" target=blk.target1 condition=blk.condition1 %}
				{% endif %}
				{% if blk.target2 or blk.condition2 %}
					{% include "_admin_survey_question_j.tpl" target=blk.target2 condition=blk.condition2 %}
				{% endif %}
			{% endfor %}
		</ul>
		<ul class="nav nav-pills" {% if is_stop %}style="display:none"{% endif %}>
			<li><a href="#jump-add">{_ Add page jump _}</a></li>
		</ul>
	</div>
	{% endwith %}
</li>
