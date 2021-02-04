{% with m.rsc[cat].id as cat %}
{% with m.rsc[predicate].name as predicate %}
{% wire
    id="dialog-new-rsc-tab"
    type="submit"
	postback={new_page
	    subject_id=subject_id
        object_id=object_id
	    predicate=predicate
	    redirect=redirect
	    actions=actions
	    callback=callback
        objects=objects
	}
	delegate=delegate|default:`action_admin_dialog_new_rsc`
%}
<form id="dialog-new-rsc-tab" method="POST" action="postback" class="form">

<div class="admin-padding">
 	{% block rsc_props_title %}
 		{# The new resource title, also used for the feedback search #}
 	    <label for="new_rsc_title">{_ Start typing a title to create a new page or find existing pages _}</label>
 	    <input type="text" id="new_rsc_title" name="title"
 	    	   value="{{ title|escape }}" class="do_autofocus form-control"
 	    	   placeholder="{_ Type title or filter existing content _}">
	{% endblock %}
</div>

<div class="new-find-cols">
	<div id="{{ #newform }}" class="form-panel">
		{% with 'dialog-new-rsc-tab' as form %}

			{% block new_rsc_header %}
 			{% endblock %}

			{% if (not nocatselect or m.category[cat].is_a.media)
				and (not predicate
						or (subject_id and m.predicate.is_valid_object_subcategory[predicate][`media`])
						or (object_id and m.predicate.is_valid_subject_subcategory[predicate][`media`]))
			%}
		        <div class="form-group" id="new-rsc-tab-upload">
		            <label class="control-label" for="upload_file">{_ Media file _}</label>
		            <div>
						<div id="upload_file_preview" style="display: none;">
							<img src="" class="thumbnail" style="max-height:200px">
						</div>
		                <input type="file" class="form-control" id="upload_file" name="upload_file">
		                <p class="help-block text-muted">
		                	<span class="glyphicon glyphicon-info-sign"></span>
		                	{_ Selecting a file will make the page a media item. _}
		                </p>
		                {% if m.category[cat].is_a.media %}
		                	{% validate id="upload_file" type={presence} %}
		                {% endif %}
		            </div>
		            {% javascript %}
		            	function file_category ( basename ) {
		            		var extension = basename.replace(/((.*)\.)*/, '');
		            		switch (extension.toLowerCase()) {
		            			// image
		            			case 'bmp': case 'jpe': case 'jpg': case 'jpeg': case 'gif': case 'png': case 'tif':
		            			case 'tiff':
		            				return 'image';
		            			// audio
		            			case 'aac': case 'aax': case 'aiff': case 'au':  case 'flac': case 'm4a': case 'm4b':
		            			case 'm4p': case 'mid': case 'midi': case 'mka': case 'mp3':  case 'oga': case 'mogg':
		            			case 'ra':  case 'ram': case 'rm':   case 'wav': case 'wma':
		            				return 'audio';
		            			// video
		            			case '3gp': case 'asf': case 'avi':  case 'm2v': case 'm4v': case 'mp2':  case 'mp4':
		            			case 'mpe': case 'mpg': case 'mpeg': case 'mov': case 'ogg': case 'webm': case 'wmv':
		            				return 'video';
		            			default:
		            				return 'document';
		            		}
			            }

		            	function is_viewable( basename ) {
		            		var extension = basename.replace(/((.*)\.)*/, '');
		            		switch (extension.toLowerCase()) {
		            			case 'jpg': return true;
		            			case 'jpeg': return true;
		            			case 'png': return true;
		            			case 'gif': return true;
		            			case 'pdf':
		            				// TODO: Works on macOS / iOS
		            				return false;
		            			default:
		            				return false;
		            		}
		            	}

		            	window.z_upload_title = '';
		            	$('#upload_file').on('change', function() {
	            			var filename = $('#upload_file').val();
	            			var basename = filename.replace(/^([^\\/]*[\\/])*/, '');
	            			var rootname = basename.replace(/\.[a-zA-Z0-9]{1,4}$/, '');
	            			var new_cat = '{{ m.rsc.media.id }}';

		            		switch (file_category(basename)) {
		            			case 'image': new_cat = '{{ m.rsc.image.id }}'; break;
		            			case 'audio': new_cat = '{{ m.rsc.audio.id }}'; break;
		            			case 'video': new_cat = '{{ m.rsc.video.id }}'; break;
		            			case 'document': new_cat = '{{ m.rsc.document.id }}'; break;
		            		}

		            		if ($('#{{ form }} select[name=category_id] option[value='+new_cat+']').length > 0) {
			            		$('#{{ form }} select[name=category_id]')
			            			.val(new_cat)
			            			.change();
		            		}

		            		if ($('#new_rsc_title').val() == '' || $('#new_rsc_title').val() == window.z_upload_title) {
		            			$('#new_rsc_title').val(rootname).change().focus();
		            			window.z_upload_title = rootname;
		            		}

		            		if (is_viewable(basename)) {
							    var reader = new FileReader();
							    reader.onload = function (e) {
							        $("#upload_file_preview img").attr('src', e.target.result);
							        $("#upload_file_preview").show();
							    };
							    reader.readAsDataURL($(this)[0].files[0]);
							} else {
						        $("#upload_file_preview").hide();
							}
		            	});
		            {% endjavascript %}

		            {# Hide/show media upload depending on the selected category #}
					{% javascript %}
						var media_cats = [
							{{ m.rsc.media.id }}
							{% for c in m.category.media.tree_flat %}
								,{{ c.id }}
							{% endfor %}
						];

						$('#{{ #newform }} [name=category_id]').on('click change', function() {
							var cat_id = $(this).val();
							var is_media = cat_id ? false : true;

							for (var i=0; i < media_cats.length; i++) {
								if (media_cats[i] == cat_id) {
									is_media = true;
								}
							}

							if (is_media) {
								$('#new-rsc-tab-upload')
									.slideDown()
									.find('input')
									.removeAttr('disabled')
									.removeClass('nosubmit');
							} else {
								$('#new-rsc-tab-upload')
									.slideUp()
									.find('input')
									.attr('disabled', true)
									.addClass('nosubmit');
							}
						});
					{% endjavascript %}
		        </div>
	        {% endif %}

			{# Category selects #}
			{% block category %}
				<div class="form-group">
				    <label class="control-label" for="{{ #category }}">{_ Category _}</label>
				    {% if cat and nocatselect %}
					    <input class="form-control" type="text" readonly value="{{ m.rsc[cat].title }}">
					    <input type="hidden" name="category_id" value="{{ cat }}">
				    {% else %}
					    {% block category_select %}
					        <select class="form-control" id="{{ #category }}" name="category_id" required>
							    <option value="" disabled {% if not cat %}selected{% endif %}>{_ Select category _}</option>
					            {% for c in m.category.tree_flat %}
					                {% if m.acl.insert[c.id.name|as_atom]
					                	  and (not subject_id or predicate|is_undefined or m.predicate.is_valid_object_category[predicate][c.id])
					                	  and (not object_id  or predicate|is_undefined or m.predicate.is_valid_subject_category[predicate][c.id])
					                %}
					                    <option value="{{c.id}}" {% if c.id == cat %}selected{% endif %}>
						                    {{ c.indent }}{{ c.id.title|default:c.id.name }}
					                    </option>
					                {% endif %}
					            {% endfor %}
					        </select>
							{% validate id=#category name="category_id" type={presence} only_on_submit %}
					    {% endblock %}
				    {% endif %}
				</div>
			{% endblock %}

			{% all include "_dialog_new_rsc_extra.tpl" %}

		    {% if cat.name == 'category' or cat.name == 'predicate' %}
			    <div class="form-group label-floating">
			        <input class="form-control" type="text" id="{{ #name }}" name="name" value="" placeholder="{_ Name _}">
				    {% validate id=#name name="name" type={presence} %}
			        <label class="control-label col-md-3" for="{{ #name }}">{_ Name _}</label>
			    </div>
		    {% endif %}

			{% block rsc_props %}
	            {% if subject_id %}
	                <div class="form-group">
                        <label class="checkbox">
                            <input type="checkbox" id="{{ #dependent }}" name="is_dependent" value="1" {% if dependent %}checked{% endif %}>
                            {_ Delete after disconnecting from _}: {{ subject_id.title }}
                        </label>
	                </div>
	            {% endif %}
				<div class="form-group">
					<label class="checkbox">
						<input type="checkbox" id="{{ #published }}" name="is_published" value="1"
							{% if subject_id or m.admin.rsc_dialog_is_published %}checked{% endif %}>
						{_ Published _}
					</label>
				</div>
			{% endblock %}

			{% block new_rsc_footer %}
			    <div class="modal-footer">
				    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
				    <button class="btn btn-primary" type="submit">
				    	{_ Create _} {{ catname }}
				    	{% if is_zlink %} &amp; {_ Link _}{% elseif subject_id or object_id %} &amp; {_ Connect _}{% endif %}
				    </button>
			    </div>
			{% endblock %}
		{% endwith %}
	</div>

	<div id="{{ #view }}" class="rsc-preview-panel-wrapper" style="display: none">
	</div>

	<div class="new-find-results">

		{# following hidden fields are for the feedback #}
		{% block feedback_query_fields %}
 			<input type="hidden" class="nosubmit" name="subject_id" value="{{ subject_id|escape }}">
 		    <input type="hidden" class="nosubmit" name="object_id" value="{{ object_id|escape }}">
 			<input type="hidden" class="nosubmit" name="predicate" value="{{ predicate|escape|default:'' }}">
 		    <input type="hidden" class="nosubmit" name="cat_exclude" value="{{ cat_exclude|flatten_value|escape }}">
 		    <input type="hidden" class="nosubmit" name="is_zlink" value="{{ is_zlink|if:'1':'' }}">
 		{% endblock %}

	    <div class="new-find-results-header">

			<h4>{_ Existing pages _}</h4>

			<p id="new-find-results-description" class="text-muted">
				{% include "_action_dialog_new_rsc_tab_find_description.tpl" %}
			</p>

			<div class="row">
				{% if not nocatselect or not cat %}
					<div class="col-sm-6">
					    {% block category_find %}
					        <select class="form-control nosubmit" id="{{ #find_category }}" name="find_category">
							    <option value="" disabled {% if not cat %}selected{% endif %}>{_ Select category _}</option>
					        	{% if predicate %}
					        		<option value="p:{{ predicate }}" selected>
						        		{_ Any valid for: _} {{ m.rsc[predicate].title }}
						        	</option>
						        	<option disabled>
						        	</option>
					        	{% else %}
					        		<option value="*" selected>
						        		{_ Any category _}
						        	</option>
						        	<option disabled>
						        	</option>
					        	{% endif %}
					            {% for c in m.category.tree_flat_meta %}
				                    <option value="{{c.id}}" {% if c.id == cat %}selected{% endif %}>
					                    {{ c.indent }}{{ c.id.title|default:c.id.name }}
				                    </option>
					            {% endfor %}
					        </select>
					    {% endblock %}
					</div>
				{% endif %}
				<div class="col-sm-6">
		        	<label class="checkbox-inline">
		        		<input type="checkbox" class="nosubmit" id="{{ #find_me }}"
		        			   name="find_creator_id" value="{{ m.acl.user }}"
		        			   {% if m.admin.connect_created_me %}checked{% endif %}>
		        		{_ Created by me _}
		        	</label>
		        </div>
	        </div>

        	{% javascript %}
        		switch (window.sessionStorage.getItem('dialog_connect_created_me')) {
        			case "true":
        				$("#{{ #find_me }}").attr('checked', true);
        				break;
        			case "false":
        				$("#{{ #find_me }}").removeAttr('checked');
        				break;
        			default:
        				break;
        		}
        		$("#{{ #find_me }}").click(function() {
        			if ($(this).is(":checked")) {
        				window.sessionStorage.setItem('dialog_connect_created_me', "true");
        			} else {
        				window.sessionStorage.setItem('dialog_connect_created_me', "false");
        			}
        		});
        	{% endjavascript %}
		</div>

		<div id="dialog-rsc-new-found" class="do_feedback"
			data-feedback="trigger: 'dialog-new-rsc-tab', delegate: 'mod_admin'">
		</div>

		{% wire name="dialog_new_rsc_preview"
		    action={update
		    	target=#view
		    	newform=#newform
		    	template="_action_dialog_new_rsc_tab_preview.tpl"
	            id=id
	            subject_id=subject_id
	            object_id=object_id
	            predicate=predicate
	            is_zlink=is_zlink
	            callback=callback
	            language=language
	            action=action
	            actions=actions
	            autoclose=autoclose
		    }
	      	action={add_class target=#view class="active"}
		    action={fade_in target=#view}
		%}
		{% wire name="dialog_new_rsc_preview_close"
	       	action={remove_class target=#view class="active"}
		    action={fade_out target=#view}
		    action={focus target="new_rsc_title"}
		%}
		{% javascript %}
		    $("#dialog-rsc-new-found")
			    .on('click', '.item .action-preview', function(e) {
			    	var select_id = $(this).closest(".item").data('id');
			    	e.preventDefault();
			    	if ($('#{{ #view }}').hasClass('active') && $('.rsc-preview-panel').attr('data-id') == select_id) {
					    $('.form-panel').transfer({ to: $(this).closest('.item'), duration: 400 });
			    		z_event('dialog_new_rsc_preview_close');
			    	} else {
					    $(this).closest('.item').transfer({ to: '.form-panel', duration: 400 });
						z_event('dialog_new_rsc_preview', {
				            select_id: select_id
				        });
			    	}
			    })
			    .on('click', '.item', function(e) {
					switch (e.target.nodeName)
					{
						case 'A':
						case 'INPUT':
							break;
						default:
					    	var select_id = $(this).data('id');
					    	e.preventDefault();
					    	if ($('#{{ #view }}').hasClass('active') && $('.rsc-preview-panel').attr('data-id') == select_id) {
							    $('.form-panel').transfer({ to: $(this), duration: 400  });
					    		z_event('dialog_new_rsc_preview_close');
					    	} else {
							    $(this).transfer({ to: '.form-panel', duration: 400 });
						        z_event('dialog_new_rsc_preview', {
						            select_id: select_id
						        });
						    }
						    break;
					}
				});

			var catInitial = '{{ cat|escape }}';

			$('#dialog-new-rsc-tab')
				.on('change', 'select[name="category_id"]', function() {
					var val = $(this).val();
					if (val !== null && val != catInitial) {
						catInitial = val;
						$('#{{ #newform }} select[name="find_category"]').val(val).change();
					}
				});

		{% endjavascript %}

		{% if subject_id or object_id or is_zlink %}
			{% wire name="dialog_new_rsc_find"
			    action={postback
			        delegate=delegate|default:`mod_admin`
			        postback={admin_connect_select
			            id=id
			            subject_id=subject_id
			            object_id=object_id
			            predicate=predicate
			            callback=callback
			            language=language
			            action=action
			            actions=actions
			            autoclose=autoclose
			            is_connect_toggle=not is_zmedia
			        }
			    }
			%}
			{% javascript %}
			    $("#dialog-new-rsc-tab")
				    .on('click', '.action-connect', function(e) {
				    	e.preventDefault();
				    	var select_id = $(this).closest(".item,.rsc-preview-panel").data('id');
				    	if (select_id) {
					    	var $item = $(".item[data-id='"+ select_id +"']");
					        z_event('dialog_new_rsc_find', {
					            select_id: select_id,
					            is_connected: $item.hasClass('item-connected')
					        });
					        $item.effect("highlight").toggleClass("item-connected");
				    	}
				    });
			{% endjavascript %}
		{% endif %}

		{# ... if in admin then redirect to edit page, otherwise redirect to view ... #}
		{% wire name="dialog_new_rsc_edit"
		    action={postback
		        delegate=`mod_admin`
		        postback={admin_rsc_redirect
		        	redirect=redirect
		            language=language
		            autoclose=autoclose
		        }
		    }
		%}
		{% javascript %}
		    $("#dialog-new-rsc-tab")
			    .on('click', '.action-edit', function(e) {
			    	e.preventDefault();
			    	var select_id = $(this).closest(".item,.rsc-preview-panel").data('id');
			    	if (select_id) {
				        z_dialog_close();
				        z_event('dialog_new_rsc_edit', {
				            select_id: select_id
				        });
				    }
			    })
			    .change();

			$('#dialog-new-rsc-tab select[name="category_id"]').change();
		{% endjavascript %}
	</div>
</div>

</form>

{% endwith %}
{% endwith %}

