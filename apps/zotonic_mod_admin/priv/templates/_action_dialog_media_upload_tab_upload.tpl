{% block upload_form %}
<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-upload">
    <p>
        {_ Upload a file from your computer. _}
        {% if not id %}
            {_ You have to specify a description of the file to make it easier to find and share. _}
        {% endif %}
    </p>

    {% wire id=#form type="submit"
        postback={media_upload
                    predicate=predicate
                    actions=actions
                    id=id
                    is_replace_medium=is_replace_medium
                    subject_id=subject_id
                    redirect=redirect|if_undefined:(not stay)
                    content_group_id=content_group_id
                    callback=callback}
        delegate=`action_admin_dialog_media_upload`
    %}
    <form id="{{ #form }}" method="POST" action="postback" class="form">
        <fieldset>
            {% if not id %}
                <div class="form-group label-floating">
                    <input type="text" class="do_autofocus form-control" id="new_media_title" name="new_media_title" value="{{ title|escape }}" placeholder="{_ Media title _}" autofocus>
                    <label class="control-label" for="new_media_title">{_ Media title _}</label>
                </div>
            {% endif %}

            <div id="{{ #upload_file_preview }}" style="display: none;">
                <img src="" class="thumbnail" style="max-height:200px">
            </div>

            <div class="form-group label-floating">
                <input type="file" class="form-control" id="{{ #upload_file }}" name="upload_file" placeholder="{_ Media file _}" required>
                <label class="control-label" for="{{ #upload_file }}">{_ Media file _}</label>
            </div>

            {% if not id %}
                {% if subject_id %}
                    <div class="form-group">
                        <div class="checkbox">
                            <label>
                                <input type="checkbox" id="{{ #dependent }}" name="is_dependent" value="1" {% if dependent %}checked{% endif %}>
                                {_ Delete after disconnecting from _} {{ subject_id.title }}
                            </label>
                        </div>
                    </div>
                {% endif %}

                <div class="form-group">
                    <div class="checkbox">
                        <label>
                            <input type="checkbox" id="{{ #published }}" name="is_published" value="1"
                                {% if subject_id or m.admin.rsc_dialog_is_published %}checked{% endif %}>
                            {_ Published _}
                        </label>
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

{% javascript %}
(function() {
    let upl = document.getElementById('{{ #upload_file }}');
    let preview = document.getElementById('{{ #upload_file_preview }}');
    let preview_img = document.querySelector('#{{ #upload_file_preview }} img')

    upl.addEventListener('change', function() {
        let files = upl.files;
        if (files.length > 0) {
            if (files[0].type.split("/")[0] == "image") {
                var reader = new FileReader();
                reader.onload = function (e) {
                    preview_img.setAttribute('src', e.target.result);
                    preview.setAttribute('style', '');
                };
                reader.readAsDataURL($(this)[0].files[0]);
            } else {
                preview.setAttribute('style', 'display: none');
            }
        }
    });
})();
{% endjavascript %}


{% endblock %}
