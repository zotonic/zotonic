<div class="item-wrapper">
	<style>
		.survey-editor ul li {
			border: 1px solid #ddd;
			border-left: 5px solid #ddd;
			padding: 4px;
			margin-bottom: 4px;
			margin-right: 4px;
			cursor: hand;
		}
		
		.survey-editor ul li textarea {
			width: 90%;
		}
		
		.survey-info {
			color: #777;
			font-style: italic;
		}
		
		#survey .shortanswer input, #survey-qs .shortanswer input {
			width: 90%;
		}
		
		#survey-qs {
			font-size: 80%;
		}
		
		#survey li {
			background-color: #f8f8f8;
			border-left-width: 1px;
		}
	</style>
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
		<span class="title">{_ Survey _}</span>
		<span class="arrow">{_ make smaller _}</span>
	</h3>
	<div class="item">
		<fieldset class="admin-form survey-editor">
			<div class="notification notice">
				{_ Below you can define your survey. Drag items from the left to the right. _} <a href="javascript:void(0)" class="do_dialog" data-dialog="title: 'Help about predicates.', text: '{_ You can create your survey by dragging the Question templates to the survey on the right. _}', width: '450px'">Need more help?</a>
			</div>

			<div class="admin-form form-item">
				<label>
					<input type="checkbox" name="survey_show_results" id="survey_show_results" value="1" {% if id.survey_show_results %}checked="checked"{% endif %} />
					{_ Show results to user after completion of survey. _}
				</label>
			</div>

			<p><a href="{% url survey_results id=id %}">{_ Show survey results _}</a></p>

			<hr/>
			
			<div class="zp-30">
				<h4>{_ Question Templates _}</h4>
				<ul id="survey-qs" style="cursor: move;">
					{% include "_admin_survey_questions.tpl" %}
				</ul>
			</div>
			
			<div class="zp-70 last">
				<h4>{_ Your survey _}</h4>
				{% sorter id="survey" tag={survey id=id} group="survey" delegate="mod_survey" %}
				<ul id="survey" style="cursor: move;" style="min-height:400px">
					{% include "_admin_survey_questions_edit.tpl" id=id %}
				</ul>
			</div>

			<hr style="clear:left" />
			{% include "_admin_save_buttons.tpl" %}
		</fieldset>
	</div>
</div>
