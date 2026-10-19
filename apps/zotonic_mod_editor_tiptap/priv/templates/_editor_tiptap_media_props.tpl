<form id="{{ #props }}" class="form" data-tiptap-media-request="{{ request_id|escape }}">
    <div class="row">
        <div class="col-md-6">
            {% live topic=id template="_editor_tiptap_media_preview.tpl" id=id %}
            <div class="form-group">
                <label for="{{ #caption }}">{_ Caption _}</label>
                <textarea id="{{ #caption }}" class="form-control" name="caption" rows="3">{{ options.caption|escape }}</textarea>
                {% block caption_help %}
                    <p class="help-block">{_ Defaults to the summary of the media. Enter a single “-” to not display a caption. _}</p>
                {% endblock %}
            </div>
        </div>
        <div class="col-md-6">
            {% block alignment %}
                <div class="form-group">
                    <label for="{{ #align }}">{_ Alignment _}</label>
                    <select id="{{ #align }}" class="form-control" name="align">
                        <option value="block" {% if options.align == 'block' %}selected{% endif %}>{_ Between text _}</option>
                        <option value="left" {% if options.align == 'left' %}selected{% endif %}>{_ Aligned left _}</option>
                        <option value="right" {% if options.align == 'right' %}selected{% endif %}>{_ Aligned right _}</option>
                    </select>
                </div>
            {% endblock %}
            {% block size %}
                <div class="form-group">
                    <label for="{{ #size }}">{_ Size _}</label>
                    <select id="{{ #size }}" class="form-control" name="size">
                        <option value="small" {% if options.size == 'small' %}selected{% endif %}>{_ Small _}</option>
                        <option value="middle" {% if options.size == 'middle' %}selected{% endif %}>{_ Medium _}</option>
                        <option value="large" {% if options.size == 'large' or not options.size %}selected{% endif %}>{_ Large _}</option>
                    </select>
                </div>
            {% endblock %}
            {% block crop %}
                <div class="checkbox"><label><input type="checkbox" name="crop" value="crop" {% if options.crop %}checked{% endif %}> {_ Crop image _}</label></div>
            {% endblock %}
            {% block class %}{% endblock %}
            {% block link %}
                <div class="checkbox"><label><input type="checkbox" name="link" value="link" {% if options.link %}checked{% endif %}> {_ Link to media or url below _}</label></div>
                <div class="checkbox"><label><input type="checkbox" name="link_new" value="new" {% if options.link_new %}checked{% endif %}> {_ Open link in new window _}</label></div>
                <div class="form-group">
                    <label for="{{ #url }}">{_ Website _}</label>
                    <input id="{{ #url }}" type="text" class="form-control" name="link_url" value="{{ options.link_url|escape }}">
                </div>
            {% endblock %}
        </div>
    </div>
    <div class="modal-footer">
        <button class="btn btn-default pull-left" type="button" name="delete">{_ Remove from text _}</button>
        {% block button_edit %}
            <button id="{{ #edit }}" class="btn btn-default pull-left" type="button">{_ Edit _}</button>
            {% wire id=#edit action={dialog_edit_basics id=id level=6 target=undefined} %}
        {% endblock %}
        <button id="{{ #cancel }}" class="btn btn-default" type="button">{_ Cancel _}</button>
        <button class="btn btn-primary" type="submit">{_ Save _}</button>
    </div>
</form>
{% wire id=#cancel action={dialog_close} %}
