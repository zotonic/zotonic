<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-upload">
    <p>
        {_ Upload a file from your computer. _}
        {% if not id %}
        {_ You have to specify a description of the file to make it easier to find and share. _}
        {% endif %}
    </p>

    {% wire id=#form type="submit"
            postback={media_upload predicate=predicate actions=actions id=id subject_id=subject_id stay=stay callback=callback}
            delegate=`action_admin_dialog_media_upload`
    %}
    <form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">
        <fieldset>
            {% if not id %}
                <div class="control-group">
                    <label class="control-label" for="new_media_title">{_ Media title _}</label>
                    <div class="controls">
                        <input type="text" class="span4 do_autofocus" id="new_media_title" name="new_media_title" value="{{ title|escape }}" />
                    </div>
                </div>
            {% endif %}

            <div class="control-group">
                <label class="control-label" for="upload_file">{_ Media file _}</label>
                <div class="controls">
                    <input type="file" class="span4" id="upload_file" name="upload_file" />
                    {% validate id="upload_file" type={presence} %}
                </div>
            </div>

            <div class="control-group">
                <label class="control-label"></label>
                <div class="controls">
                    <label class="checkbox">
                        <input type="checkbox" id="{{ #published }}" name="is_published" value="1" 
                             {% if subject_id or m.config.mod_admin.rsc_dialog_is_published.value %}checked="checked"{% endif %}
                        />
                        {_ Published _}
                    </label>
                    {% if in_sorter|match:"^menu" or subject_id %}
                        <label class="checkbox">
                            <input type="checkbox" id="{{ #dependent }}" name="is_dependent" value="1" checked />
                            {% if subject_id %}
                                {_ Delete after disconnecting from _} {{ subject_id.title }}
                            {% else %}
                                {_ Delete after removal from menu _}
                            {% endif %}
                        </label>
                    {% endif %}
                </div>
            </div>

            <div class="modal-footer">
                {% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}
                <button class="btn btn-primary" type="submit">{_ Upload file _}</button>
            </div>
        </fieldset>
    </form>
</div>
