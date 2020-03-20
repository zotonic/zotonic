{% block upload_form %}
<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-upload">
    <p>
        {_ Upload a file from your computer. _}
        {% if not id %}
            {_ You have to specify a description of the file to make it easier to find and share. _}
        {% endif %}
    </p>

    {% wire id=#form type="submit"
        postback={media_upload predicate=predicate actions=actions id=id subject_id=subject_id 
                    redirect=redirect|if_undefined:(not stay) content_group_id=content_group_id callback=callback}
        delegate=`action_admin_dialog_media_upload`
    %}
    <form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">
        <fieldset>
            {% if not id %}
                <div class="form-group row">
                    <label class="control-label col-md-3" for="new_media_title">{_ Media title _}</label>
                    <div class="col-md-9">
                        <input type="text" class="do_autofocus col-lg-4 col-md-4 form-control" id="new_media_title" name="new_media_title" value="{{ title|escape }}" />
                    </div>
                </div>
            {% endif %}

            <div class="form-group row">
                <label class="control-label col-md-3" for="{{ #upload_file }}">{_ Media file _}</label>
                <div class="col-md-9">
                    <input type="file" class="form-control" id="{{ #upload_file }}" name="upload_file" />
                    {% validate id=#upload_file name="upload_file" type={presence} %}
                </div>
            </div>

            {% if not id %}
                {% if subject_id %}
                    <div class="form-group row">
                        <div class="col-md-9 col-md-offset-3">
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox" id="{{ #dependent }}" name="is_dependent" value="1">
                                    {_ Delete after disconnecting from _} {{ subject_id.title }}
                                </label>
                            </div>
                        </div>
                    </div>
                {% endif %}

                <div class="form-group row">
                    <div class="col-md-9 col-md-offset-3">
                        <div class="checkbox">
                            <label>
                                <input type="checkbox" id="{{ #published }}" name="is_published" value="1"
                                    {% if subject_id or m.config.mod_admin.rsc_dialog_is_published.value %}checked{% endif %}>
                                {_ Published _}
                            </label>
                        </div>
                    </div>
                </div>
            {% endif %}

            <div class="modal-footer">
                {% button class="btn btn-default" action={dialog_close} text=_"Cancel" %}
                {% button class="btn btn-primary" type="submit" text=_"Upload file" %}
            </div>
        </fieldset>
    </form>
</div>
{% endblock %}
