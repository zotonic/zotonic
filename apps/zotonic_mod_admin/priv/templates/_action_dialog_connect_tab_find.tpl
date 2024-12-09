{% with m.predicate.object_category[predicate] as ocats %}
{% if subject_id and m.rsc[predicate].is_connect_checkbox and ocats %}
    {# Keywords shown as list with checkboxes #}
    {% with m.rsc[subject_id].o[predicate] as oids %}
    <div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-find">

        <form>
            <input type="text"
                   autofocus
                   class="form-control do_listfilter do_autofocus"
                   placeholder="{_ Type to filter the list below. _}"
                   data-listfilter='{ "method": "words", "list": "#find-connect-objects .checkbox" }'
            >
        </form>

        <div id="find-connect-objects">
            {% for cat in m.predicate.object_resources[predicate] %}
                {% if ocats|length > 1 %}
                    <h4 class="text-muted">{{ cat.category.title }}</h4>
                {% endif %}

                <div class="checkbox-list" {% if not forloop.last %}style="border-bottom: 1px solid #e5e5e5"{% endif %}>
                    {% for id in cat.resources %}
                        {% if id.is_visible %}
                            <label class="checkbox">
                                <input type="checkbox" value="{{ id }}" {% if id|member:oids %}checked{% endif %}>
                                {{ id.title|default:id.short_title }}
                            </label>
                        {% endif %}
                    {% endfor %}
                    {# Padding for flexbox last row #}
                    <label class="checkbox"></label>
                    <label class="checkbox"></label>
                    <label class="checkbox"></label>
                </div>
            {% endfor %}
        </div>

        <div class="modal-footer">
            <a class="btn btn-default" id="{{ #close }}">
                {% if autoclose %}{_ Cancel _}{% else %}{_ Close _}{% endif %}
            </a>
            {% wire id=#close action={dialog_close} %}
        </div>

        {% wire name="dialog_connect_find"
            action={postback
                delegate=delegate|default:`mod_admin`
                postback={admin_connect_select
                    intent=intent
                    id=id
                    subject_id=subject_id
                    object_id=object_id
                    predicate=predicate
                    callback=callback
                    language=language
                    action=action
                    actions=actions
                    autoclose=false
                    is_connect_toggle=true
                }
            }
        %}
        {% javascript %}
            $('#{{ tab }}-find input[type=checkbox]').on('click', function() {
                z_event('dialog_connect_find', {
                    select_id: $(this).val(),
                    is_connected: !$(this).is(":checked")
                });
            });
        {% endjavascript %}
    </div>
    {% endwith %}
{% else %}
    {# Search box and list items #}
    <div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-find">
    	<form id="dialog-connect-find" class="row form form-horizontal">
    		<input type="hidden" name="subject_id" value="{{ subject_id }}" />
            <input type="hidden" name="object_id" value="{{ object_id }}" />
    		<input type="hidden" name="predicate" value="{{ predicate|default:'' }}" />
            <input type="hidden" name="intent" value="{{ intent|escape }}" />

            <div class="col-md-6">
    		    <input name="find_text" type="text" value="{{ text|default:'' }}" placeholder="{_ Type text to search _}" class="do_autofocus form-control" />
            </div>

            <div class="col-md-3">
    		    {% block category_select %}
                    {% if nocatselect %}
                        <input type="hidden" name="find_category" value="{{ cat.id }}" />
                    {% else %}
        		        <select class="form-control" name="find_category">
        			        {% if predicate %}
        				        <option value="p:{{ predicate }}" selected="selected">{_ Valid for: _} {{ predicate.title }}</option>
        			        {% endif %}
        			        <option value="">{_ Any category _}</option>
        			        <option value="" disabled></option>
                            {% if cat.is_a.meta %}
                                {% for c in m.category.tree_flat_meta %}
                                    <option value="{{ c.id }}" {% if c.id == cat and not predicate %}selected="selected"{% endif %}>
                                        {{ c.indent }}{{ c.id.title|default:c.id.name }}
                                    </option>
                                {% endfor %}
                            {% else %}
            		            {% for c in m.category.tree_flat %}
            			            <option value="{{ c.id }}" {% if c.id == cat and not predicate %}selected="selected"{% endif %}>
            					        {{ c.indent }}{{ c.id.title|default:c.id.name }}
            			            </option>
            		            {% endfor %}
                            {% endif %}
        		        </select>
                    {% endif %}
    	        {% endblock %}
            </div>

            <div class="col-md-3">
                {% with m.acl.user.s.hascollabmanager as cmgr %}
                {% with m.acl.user.s.hascollabmember as cmbr %}
                    <select class="form-control" name="find_cg" id="{{ #find_cg }}">
                        <option value="">{_ Anybody’s _}</option>
                        <option value="me" {% if content_group == 'me' %}selected{% endif %}>{_ Mine _}</option>
                        {% if cmgr or cmbr %}
                            <optgroup label="{_ Collaboration groups _}">
                                {% for cid in cmgr ++ (cmbr -- cmgr) %}
                                    {% if cid.is_visible %}
                                        <option value="{{ cid }}" {% if cid == content_group %}selected{% endif %}>{{ cid.title }}</option>
                                    {% endif %}
                                {% endfor %}
                            </optgroup>
                        {% endif %}
                        <optgroup label="{_ Content groups _}">
                            {% for c in m.hierarchy.content_group.tree_flat %}
                                {% if c.id.is_visible %}
                                    <option value="{{ c.id }}"  {% if c.id == content_group %}selected{% endif %}>
                                        {{ c.indent }} {{ c.id.title }}
                                    </option>
                                {% endif %}
                            {% endfor %}
                        </optgroup>
                    </select>
                {% endwith %}
                {% endwith %}

                {% javascript %}
                    switch (window.sessionStorage.getItem('dialog_connect_created_me')) {
                        case "true":
                            $("#{{ #find_cg }}").val('me');
                            break;
                        case "false":
                            break;
                        default:
                            {% if m.admin.connect_created_me and not content_group %}
                                $("#{{ #find_cg }}").val('me');
                            {% endif %}
                            break;
                    }
                    $("#{{ #find_cg }}").change(function() {
                        if ($(this).val() == 'me') {
                            window.sessionStorage.setItem('dialog_connect_created_me', "true");
                        } else {
                            window.sessionStorage.setItem('dialog_connect_created_me', "false");
                        }
                    });
                {% endjavascript %}
            </div>
    	</form>

    	<div id="dialog-connect-found" class="do_feedback"
    		data-feedback='{ "trigger": "dialog-connect-find", "delegate": "mod_admin" }'>
    	</div>

        <div class="modal-footer">
            <a class="btn btn-default" id="{{ #close }}">
                {% if autoclose %}{_ Cancel _}{% else %}{_ Close _}{% endif %}
            </a>
            {% wire id=#close action={dialog_close} %}
        </div>
    </div>
    {% wire name="dialog_connect_find"
        action={postback
            delegate=delegate|default:`mod_admin`
            postback={admin_connect_select
                intent=intent
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
        $('#dialog-connect-find').submit(function() { return false; });
        $('#dialog-connect-find').change();
        $("#dialog-connect-found").on('click', '.thumbnail', function(e) {
        	e.preventDefault();
            z_event('dialog_connect_find', {
                select_id: $(this).data('id'),
                is_connected: $(this).hasClass('thumbnail-connected')
            });
            $(this).effect("highlight").toggleClass("thumbnail-connected");
        });
    {% endjavascript %}
{% endif %}
{% endwith %}
