<div id="tinyinit">
	{% lazy action={script script="z_tinymce_init();"} action={remove target="tinyinit"} %}
</div>

{% with id.blocks|survey_as_pages as pages %}
	<div id="admin-survey-questions">
		<ol class="pages">
			{% for qs,js in pages %}
				{% include "_admin_survey_question_page.tpl" %}
			{% endfor %}
		</ol>
		<ul class="nav nav-pills">
			<li><a href="#page-append">{_ Add page _}</a></li>
		</ul>
	</div>
{% endwith %}

	<!-- Block select dialog -->

	<div id="modal-question" class="modal hide">
		<div class="modal-header">
			<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
			<h3>{_ Add a question or block _}</h3>
		</div>
		<div class="modal-body">
			<div class="row-fluid">
				<div class="span4" data-block-type='survey_yesno'>
					<p>{_ Do you like pea soup? _}</p>
					<p>
						<input type="radio" disabled /> {_ Yes _}<br/>
						<input type="radio" disabled /> {_ No _}
					</p>
				</div>
				<div class="span4" data-block-type='survey_truefalse'>
					<p>{_ The earth is flat. _}</p>
					<p>
						<input type="radio" disabled /> {_ True _}<br/>
						<input type="radio" disabled /> {_ False _}
					</p>
				</div>
				<div class="span4" data-block-type='survey_likert'>
					<p>{_ Weasels make great pets. _}</p>
					<p>
						{_ Disagree _}
						<input type="radio" disabled />
						<input type="radio" disabled />
						<input type="radio" disabled />
						<input type="radio" disabled />
						<input type="radio" disabled />
						{_ Agree _}
					</p>
				</div>
			</div>
			<hr/>
			<div class="row-fluid">
				<div class="span4" data-block-type='survey_matching'>
					<p>{_ Match which answer fits best. _}</p>
					<p>
						Apple <select disabled><option>{_ Wagner _}</option></select><br/>
						Milk <select disabled><option>{_ Red _}</option></select><br/>
					</p>
				</div>
				<div class="span4" data-block-type='survey_thurstone'>
					<p>{_ Select which you agree with. _}
					<p>
						<input type="checkbox" disabled /> {_ I like Chinese restaurants _}<br/>
						<input type="checkbox" disabled /> {_ Chinese food is best for money _}<br/>
						<input type="checkbox" disabled /> {_ I always eat with others _}
					</p>
				</div>
				<div class="span4" data-block-type='survey_narrative'>
					<p>
						{_ I am _}
						<input type="text" class="input-mini" disabled />
						{_ years old. I like _}
						<select class="input-small" disabled><option>{_ chocolate _}</option></select>
						{_ ice cream and my favorite color is _} <input type="text" class="input-mini" disabled />.
					</p>
				</div>
			</div>
			<hr/>
			<div class="row-fluid">
				<div class="span4" data-block-type='survey_category'>
					<p>{_ Options from a page category _}</p>
					<p>
						<input type="checkbox" disabled /> {_ First page in category _}<br/>
						<input type="checkbox" disabled /> {_ Second page in category _}<br/>
						<input type="checkbox" disabled /> {_ Next page in category _}
					</p>
				</div>
				<div class="span4" data-block-type='survey_country'>
					<p>{_ Select your country _}</p>
					<p>
						<select disabled>
							<option disabled>{_ Country _}</option>
						</select>
					</p>
				</div>
				<div class="span4" data-block-type='survey_upload'>
					<p>{_ Please upload your image. _}</p>
					<p>
						<input style="width:95%" type="file" disabled />
					</p>
				</div>
			</div>
			<hr/>
			<div class="row-fluid">
				<div class="span4" data-block-type='survey_button'>
					<p>
						<button class="btn disabled" disabled>{_ Button text _}</button>
					</p>
				</div>
				<div class="span4" data-block-type='survey_short_answer'>
					<p>{_ Please enter your name. _}</p>
					<p>
						<input type="text" class="input-block-level" disabled />
					</p>
				</div>
				<div class="span4" data-block-type='survey_long_answer'>
					<p>{_ Please write an essay. _}</p>
					<p>
						<textarea rows="3" class="input-block-level" disabled></textarea>
					</p>
				</div>
			</div>
			<hr/>
			<div class="row-fluid">
				<div class="span4" data-block-type='header'>
					<h4>{_ Header _}</h4>
				</div>
				<div class="span4" data-block-type='text'>
					<p>{_ Lorem ipsum dolor sit amet, consectetur <em>adipisicing</em> elit, sed do eiusmod <b>tempor incididunt</b> ut labore et dolore <u>magna aliqua</u>. _}</p>
				</div>
				<div class="span4" data-block-type='page'>
					{% image "lib/images/koe.jpg" mediaclass="admin-editor" grey %}
				</div>
			</div>
		</div>
		<div class="modal-footer">
			<a href="#" class="btn" data-dismiss="modal">{_ Cancel _}</a>
		</div>
	</div>

	<!-- Templates -->

	<div id="page-template" class="hide">
		<ul>
			{% include "_admin_survey_question_page.tpl" qs=[] js=[] nosubmit %}
		</ul>
	</div>

	<div id="question-template" class="hide">
		<ul>
			{% include "_admin_survey_question_q.tpl" blk=[] nosubmit %}
		</ul>
	</div>

	<div id="jump-template" class="hide">
		<ul>
			{% include "_admin_survey_question_j.tpl" blk=[] nosubmit %}
		</ul>
	</div>

{% javascript %}
	$('#admin-survey-questions').on('click', '.block-page a.page-connect', function(event) {
	    window.zBlockConnectTrigger = this;
	    z_event("admin-q-block-connect", {});
	    event.preventDefault();
	});

	window.zAdminBlockConnectDone = function(v) {
	    var $block_page = $(window.zBlockConnectTrigger).closest(".block-page");
	    var target_id = $(".rsc-item-wrapper", $block_page).attr('id');
	    $("input[type=hidden]", $block_page).val(v.object_id);
	    z_notify("update", {z_delegate: 'mod_admin', template: "_rsc_item.tpl", id: v.object_id, z_target_id: target_id});
	    window.zAdminConnectDone(v);
	}

	$('#admin-survey-questions').on('click', '.rsc-item h5 a', function(event) {
	    var rsc_id = $(this).attr('href').replace('#', '');
	    z_event("admin-edit-basics", {
	                        id: rsc_id, 
	                        element_id: $(this).closest(".rsc-item").attr('id'), 
	                        template: "_rsc_item.tpl", 
	                        edit_dispatch: "{{ edit_dispatch }}"
	                });
	    event.preventDefault();
	});
{% endjavascript %}

{% wire name="admin-q-block-connect" 
        action={dialog_open
                    subject_id=id
                    predicate=""
                    template="_action_dialog_connect.tpl" 
                    title=_"Find page"
                    callback="window.zAdminBlockConnectDone"}
%}

{% wire name="admin-edit-basics" action={dialog_edit_basics template="_rsc_item.tpl"} %}
