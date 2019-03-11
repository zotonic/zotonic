{% with m.rsc[cat].name|as_atom as cat %}
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
<form id="dialog-new-rsc-tab" method="POST" action="postback" class="form form-horizontal">

<div class="row">
<div class="col-md-6">
	{% with 'dialog-new-rsc-tab' as form %}

		{# The new resource title, also used for the feedback search #}
		<div class="form-group row">
		    <label class="control-label col-md-3" for="new_rsc_title">{_ Page title _}</label>
		    <div class="col-md-9">
			    <input type="text" id="new_rsc_title" name="title" value="{{ title|escape }}" class="do_autofocus form-control" />
			    {% validate id="new_rsc_title" name="title" type={presence} %}
		    </div>
		</div>

		{% if (not nocatselect or m.category[cat].is_a.media)
			and (not predicate
					or (subject_id and m.predicate.is_valid_object_subcategory[predicate][`media`])
					or (object_id and m.predicate.is_valid_subject_subcategory[predicate][`media`]))
		%}
	        <div class="form-group row">
	            <label class="control-label col-md-3" for="upload_file">{_ Media file _}</label>
	            <div class="col-md-9">
					<div id="upload_file_preview" style="display: none;">
						<img src="" class="thumbnail" style="max-height:200px">
					</div>
	                <input type="file" class="form-control" id="upload_file" name="upload_file" />
	                <p class="help-block">
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
	            			$('#new_rsc_title').val(basename).change();
	            			window.z_upload_title = basename;
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
	        </div>
        {% endif %}

		{# Category selects #}
		{% block category %}
			<div class="form-group row">
			    <label class="control-label col-md-3" for="{{ #category }}">{_ Category _}</label>
			    <div class="col-md-9">
				    {% if cat and nocatselect %}
					    <input class="form-control" type="text" readonly value="{{ m.rsc[cat].title }}" />
					    <input type="hidden" name="category_id" value="{{ cat }}"/>
				    {% else %}
					    {% block category_select %}
					        <select class="form-control" id="{{ #category }}" name="category_id">
								<option></option>
					            {% for c in m.category.tree_flat %}
					                {% if m.acl.insert[c.id.name|as_atom]
					                	  and (not cat or m.category[c.id].is_a[cat])
					                	  and (not subject_id or m.predicate.is_valid_object_category[predicate][c.id])
					                	  and (not object_id or m.predicate.is_valid_subject_category[predicate][c.id])
					                %}
					                    <option value="{{c.id}}" {% if c.id == cat %}selected="selected" {% endif %}>
						                    {{ c.indent }}{{ c.id.title|default:c.id.name }}
					                    </option>
					                {% endif %}
					            {% endfor %}
					        </select>
							{% validate id=#category name="category_id" type={presence} %}
					    {% endblock %}
				    {% endif %}
			    </div>
			</div>
		{% endblock %}

		{% all include "_dialog_new_rsc_extra.tpl" %}

	    {% if cat.name == 'category' or cat.name == 'predicate' %}
		    <div class="form-group row">
		        <label class="control-label col-md-3" for="{{ #name }}">{_ Name _}</label>
		        <div class="col-md-9">
			        <input class="form-control" type="text" id="{{ #name }}" name="name" value="" />
				    {% validate id=#name name="name" type={presence} %}
		        </div>
		    </div>
	    {% endif %}

		{% block rsc_props %}
			<div class="form-group row">
			    <label class="control-label col-md-3" for="{{ #published }}"></label>
			    <div class="checkbox col-md-9">
				    <label>
				        <input type="checkbox" id="{{ #published }}" name="is_published" value="1"
						    {% if subject_id or m.config.mod_admin.rsc_dialog_is_published.value %}checked="checked"{% endif %} />
						{_ Published _}
				    </label>
			    </div>
			</div>
		{% endblock %}

	    <div class="modal-footer">
		    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
		    <button class="btn btn-primary" type="submit">{_ Make _} {{ catname }}</button>
	    </div>
	{% endwith %}
</div>
<div class="col-md-6">

	{# following hidden fields are for the feedback #}
	<input type="hidden" class="nosubmit" name="subject_id" value="{{ subject_id }}" />
    <input type="hidden" class="nosubmit" name="object_id" value="{{ object_id }}" />
	<input type="hidden" class="nosubmit" name="predicate" value="{{ predicate|default:'' }}" />

	<div class="row">
        {% if predicate %}
        	<div class="col-xs-6">
	        	<label class="checkbox-inline">
	        		<input type="checkbox" class="nosubmit" name="find_category" value="p:{{ predicate }}" checked />
	        		{_ Any valid for: _} {{ m.rsc[predicate].title }}
	        	</label>
			</div>
        {% endif %}
    	<div class="col-xs-6">
        	<label class="checkbox-inline">
        		<input type="checkbox" class="nosubmit" name="find_cg" value="me" />
        		{_ My content _}
        	</label>
        </div>
    </div>

	<p class="help-block">
		{_ Existing pages matching the title. _}
		{% if subject_id or object_id %}
			{_ Click to connect. _}
		{% else %}
			{_ Click to view. _}
		{% endif %}
	</p>

	<div id="dialog-rsc-new-found" class="do_feedback"
		data-feedback="trigger: 'dialog-new-rsc-tab', delegate: 'mod_admin'">
	</div>

	{% if subject_id or object_id %}
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
		        }
		    }
		%}
		{% javascript %}
		    $("#dialog-rsc-new-found")
			    .on('click', '.thumbnail', function(e) {
			    	e.preventDefault();
			        z_event('dialog_new_rsc_find', {
			            select_id: $(this).data('id'),
			            is_connected: $(this).hasClass('thumbnail-connected')
			        });
			        $(this).effect("highlight").toggleClass("thumbnail-connected");
			    })
			    .change();
		{% endjavascript %}
	{% else %}
		{# ... if in admin then redirect to edit page, otherwise redirect to view ... #}
		{% wire name="dialog_new_rsc_find"
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
		    $("#dialog-rsc-new-found")
			    .on('click', '.thumbnail', function(e) {
			    	e.preventDefault();
			        z_dialog_close();
			        z_event('dialog_new_rsc_find', {
			            select_id: $(this).data('id'),
			        });
			    })
			    .change();
		{% endjavascript %}
	{% endif %}
</div>
</div>

</form>

{% endwith %}

