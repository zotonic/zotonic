{% javascript %}
	$('.tree-list .active')
		.removeClass('active');
	$('.tree-list div[data-page-id="{{ id }}"]')
		.closest('.menu-item')
		.addClass('active');
	$('#save-buttons')
		.hide()
		.fadeIn();
{% endjavascript %}

{% block tinymce_init %}
	{% catinclude "_admin_frontend_tinymce_init.tpl" id tree_id=tree_id %}
{% endblock %}

{% block rscform %}
{% if id.exists %}
	{% with id.is_editable as is_editable %}
	{% with id.is_a|default:(m.category[cat].is_a) as cats %}
	{% wire id="rscform"
			type="submit"
			postback={rscform
				view_location=view_location
				on_success=on_save
			}
			delegate=`controller_admin_edit`
	%}
	<form id="rscform" method="post" action="postback" class="form do_formdirty">
		<input type="hidden" name="id" value="{{ id }}">

		<div class="meta-data row">
			<div class="col-lg-10 col-md-10">
				{% block meta_data_first %}{% endblock %}

				<div>
					{% block meta_data_title %}
						<h3>
							{% if id %}
								{{ id.category_id.title }}
							{% else %}
								{{ m.rsc[cat].title }}
							{% endif %}
						</h3>
					{% endblock %}
					<span class="publication-dates">
						<label for="is_published" class="checkbox-inline">
				    		<input type="checkbox" id="is_published" name="is_published" value="1" {% if not id or id.is_published %}checked="checked"{% endif %}/>
				    	    {_ Published _}
					    </label>

						{% include "_edit_date.tpl" date=id.publication_start name="publication_start" is_end=0 %}
						{% if id.is_editable or id.publication_end %}
							{_ till _}
							{% include "_edit_date.tpl" date=id.publication_end name="publication_end" is_end=1 %}
						{% endif %}
						{{ m.req.timezone }}
					</span>
				</div>
			</div>
		</div>

		{% block meta_data_after %}
		{% endblock %}

		<ul class="nav nav-tabs">
			<li class="active"><a href="#poststuff" data-toggle="tab">{_ Content _}</a></li>
			{% block meta_tabs %}{% endblock %}
			{% if m.modules.active.mod_translation %}
				<li><a href="#meta-language" data-toggle="tab">{_ Language _}</a></li>
			{% endif %}
			<li><a href="#meta-acl" data-toggle="tab">{_ Access control _}</a></li>
		</ul>
		<div class="tab-content">
			<div class="tab-pane active" id="poststuff">
				{% optional include "_translation_init_languages.tpl" %}
				{% block edit_blocks %}
					{% catinclude "_admin_edit_basics.tpl" cats %}

					{% if id.category_id.is_feature_show_address|if_undefined:true %}
						{% catinclude "_admin_edit_content_address.tpl" cats %}
					{% endif %}

					{% all catinclude "_admin_edit_content.tpl" cats %}

					{% if `media`|member:cats or id.medium %}
						{% include "_admin_edit_content_media.tpl" %}
					{% endif %}

					{% catinclude "_admin_edit_body.tpl" cats %}
					{% catinclude "_admin_edit_blocks.tpl" cats %}
					{% catinclude "_admin_edit_depiction.tpl" cats %}
				{% endblock %}
			</div>
			{% block meta_panels %}{% endblock %}
			{% block meta_panel_language %}
				<div class="tab-pane" id="meta-language">
					<div class="widget">
					    <div class="widget-header">
					        {_ Language _}
					    </div>
					    <div class="widget-content">
					    	<p class="help-block">{_ Enabled or disable language tabs. _}</p>
							{% optional include "_translation_edit_languages.tpl" %}

					    	<p class="help-block">{_ Add a translation or copy texts between languages. _}</p>
							<div class="padding" style="padding-top: 0">
								{% include "_admin_translation_tabs_extra.tpl" button %}
							</div>
						</div>
					</div>
				</div>
			{% endblock %}
			{% block meta_panel_access_control %}
				<div class="tab-pane" id="meta-acl">
					<div class="widget">
					    <div class="widget-header">
					        {_ Access control _}
					    </div>
					    <div class="widget-content">
							{% optional include "_admin_edit_visible_for.tpl" id=id is_admin_frontend %}
						</div>
					</div>
				</div>
			{% endblock %}
		</div>

		{# Hidden safe buttons and publish state - controlled via the nabvar #}
		<div style="display: none">
			<span id="button-prompt">
				{% block nav_prompt %}
					{{ id.category_id.title }}
				{% endblock %}
			</span>

			{% block buttons %}
				{% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}

				{% if id.is_editable %}
					{% button type="submit" id="save_view" class="btn btn-default" text=_"Save &amp; view" title=_"Save and view the page." %}
				{% elseif id %}
					{% button id="save_view" class="btn btn-primary" text=_"View" title=_"View this page." action={redirect id=id} %}
				{% endif %}
			{% endblock %}
		</div>
	</form>
	{% endwith %}
	{% endwith %}

	{% javascript %}
		$("#save-buttons .brand").html($('#button-prompt').html());

		setTimeout(function() {
	 	    $('#rscform').on('shown.bs.tab', '.language-tabs > li > a[data-toggle="tab"]', function (e) {
				if (e.target != e.relatedTarget) {
			        const showLang = e.target.closest('li').getAttribute('lang');
			        const hideLang = e.relatedTarget.closest('li').getAttribute('lang');
					$("li[lang='"+showLang+"']:visible > a").tab('show');

					// Also switch language dependent parts that are not inside the tab panes.
					$(".widget-content-lang-" + hideLang).hide()
					$(".widget-content-lang-" + showLang).show();

					setTimeout( () => z_editor.init(), 1 );
				}
			});
			{% if tab %}
				$("li a[href='#{{ tab|escape }}'").tab("show");
			{% endif %}
		}, 10);
	{% endjavascript %}
{% else %}
	<h1 class="text-muted">{_ No page _}</h1>
	<p class="text-muted">
		{_ The page might have been deleted. _}
	</p>
{% endif %}

{% endblock %}

