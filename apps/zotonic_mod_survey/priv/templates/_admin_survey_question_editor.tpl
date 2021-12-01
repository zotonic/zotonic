<div id="tinyinit" style="padding: 10px 10px 0 30px;">
	<div style="display: inline-block">
		{% lazy action={script script="z_editor_init();"} action={remove target="tinyinit"} %}
	</div>
	{_ Initializing... _}
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

<div id="modal-question" class="modal fade" tabindex="-1" role="dialog">
    <div class="modal-dialog modal-lg" role="document">
        <div class="modal-content">
			<div class="modal-header">
				<button type="button" class="close" data-dismiss="modal" aria-hidden="true"></button>
				<h3>{_ Add a question or block _}</h3>
			</div>
			<div class="modal-body">
				<div class="row">
					<div class="col-lg-4 col-md-4" data-block-type='survey_short_answer'>
						<p>{_ Question with a short answer _} ({_ text _}, {_ number _}, {_ email _}, {_ phone number _}, {_ date _})</p>
						<p>
							<input class="form-control" type="text" disabled />
						</p>
					</div>
					<div class="col-lg-4 col-md-4" data-block-type='survey_long_answer'>
						<p>{_ Question with a long answer _}</p>
						<p>
							<textarea class="form-control" rows="3" disabled></textarea>
						</p>
					</div>
					<div class="col-lg-4 col-md-4" data-block-type='survey_yesno'>
						<p>{_ Yes or no question _}</p>
						<p>
							<input type="radio" disabled /> {_ Yes _}<br/>
							<input type="radio" disabled /> {_ No _}
						</p>
					</div>
					<!--
					<div class="col-lg-4 col-md-4" data-block-type='survey_truefalse'>
						<p>{_ True or false question _}</p>
						<p>
							<input type="radio" disabled /> {_ True _}<br/>
							<input type="radio" disabled /> {_ False _}
						</p>
					</div>
					-->
				</div>
				<hr/>
				<div class="row">
					<div class="col-lg-4 col-md-4" data-block-type='survey_likert'>
						<p>{_ Question with a 5-point scale _} (Likert)</p>
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
					<div class="col-lg-4 col-md-4" data-block-type='survey_thurstone'>
						<p>{_ Multiple choice or quiz question _} (Thurstone)</p>
						<p>
							<input type="checkbox" disabled /> {_ Answer _} 1<br/>
							<input type="checkbox" disabled /> {_ Answer _} 2<br/>
							<input type="checkbox" disabled /> {_ Answer _} 3
						</p>
					</div>
					<div class="col-lg-4 col-md-4" data-block-type='survey_matching'>
						<p>{_ Matching question _}</p>
						<p>
							{_ Apple _}<select class="form-control" disabled><option>{_ Wagner _}</option></select><br/>
							{_ Melk _} <select class="form-control" disabled><option>{_ Red _}</option></select><br/>
						</p>
					</div>
				</div>
				<hr/>
				<div class="row">
					<div class="col-lg-4 col-md-4" data-block-type='survey_narrative'>
						<div class="form-inline">
							{_ Fill in the gaps _}  (Narrative)</p><br>{_ I am _}
							<input type="text" class="form-control" style="display: inline; float: none;" disabled>
							{_ years old. I like _}
							<select class="form-control" style="display: inline; float: none;" disabled><option>{_ chocolate _}</option></select>
							{_ ice cream and my favorite color is _} <input type="text" class="form-control" style="display: inline; float: none;" disabled>.
						</div>
					</div>
					<div class="col-lg-4 col-md-4" data-block-type='survey_country'>
						<p>{_ Select your country _}</p>
						<p>
							<select class="form-control" disabled>
								<option disabled>{_ Country _}</option>
							</select>
						</p>
					</div>
					<div class="col-lg-4 col-md-4" data-block-type='survey_button'>
						<p>{_ Button _}<br/>{_ Create jumps to other questions or blocks. _}<br/>
							<button class="btn btn-default disabled" disabled>{_ Button text _}</button>
						</p>
					</div>
				</div>
				<hr/>
				<div class="row">
					<div class="col-lg-4 col-md-4" data-block-type='header'>
						<h4>{_ Header _}</h4>
					</div>
					<div class="col-lg-4 col-md-4" data-block-type='text'>

						<p>{_ Text, image, video _}<br/><br/>{_ Add an introduction or explanation including uploaded or embedded images or video's. Add full width blocks or set a percentage to combine blocks. _}</p>
					</div>
				</div>
				{% if m.acl.use.mod_admin %}
				<hr/>
				<div class="alert">
					<div class="row">
						<div class="col-lg-4 col-md-4" data-block-type='survey_category'>
							<p>{_ Options from a page category _}</p>
							<p>
								<input type="checkbox" disabled /> {_ First page in category _}<br/>
								<input type="checkbox" disabled /> {_ Second page in category _}<br/>
								<input type="checkbox" disabled /> {_ Next page in category _}
							</p>
						</div>
						<div class="col-lg-4 col-md-4" data-block-type='survey_upload'>
							<p>{_ Please upload your file. _}</p>
							<p>
								<input class="form-control" style="width:95%" type="file" disabled />
							</p>
						</div>
						<div class="col-lg-4 col-md-4" data-block-type='page'>
							<p>{_ Another page, video or image. _}</p>
							{% image "lib/images/koe.jpg" mediaclass="admin-editor" grey %}
						</div>
					</div>
				</div>
				{% endif %}
			</div>
			<div class="modal-footer">
				<a href="#" class="btn btn-default" data-dismiss="modal">{_ Cancel _}</a>
			</div>
		</div>
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
		<li class="block" id="{{ #s }}">
			<!-- The template _admin_survey_question_q.tpl will be inserted here -->
		</li>
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
	    z_notify("update", {z_delegate: 'mod_admin', id: v.object_id, z_target_id: target_id});
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
        			intent="select"
                    subject_id=id
                    predicate=""
                    template="_action_dialog_connect.tpl"
                    title=_"Find page"
                    callback="window.zAdminBlockConnectDone"
                    autoclose
                    is_zlink
                    width="large"
                }
%}

{% wire name="admin-edit-basics" action={dialog_edit_basics template="_rsc_item.tpl"} %}
