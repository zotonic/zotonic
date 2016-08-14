{# Panel for defining the embed code #}
{% if not tabs_enabled or "oembed"|member:tabs_enabled %}

{% with id.medium as medium %}
{% with medium.mime == "text/html-oembed" as is_oembed %}
<div class="tab-pane" id="{{ tab }}-oembed">
    <p>{_ Embed a video or other media URL. Here you can paste any URL from YouTube, Vimeo or other services. _}</p>

    {% wire id=#form type="submit"
        postback={add_video_embed predicate=predicate actions=actions id=id
                                subject_id=subject_id stay=stay callback=callback}
        delegate="mod_oembed"
    %}
    <form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">

        <div class="form-group row">
            <label class="control-label col-md-3" for="{{ #embed_code }}">{_ Embed URL _}</label>
            <div class="col-md-6">
                <input id="{{ #embed_code }}" class="form-control do_autofocus" name="oembed_url" value="{{ medium.oembed_url }}" placeholder="http://www.youtube.com/watch?v=r9cmWJvXIj4‎" />
                {% validate id=#embed_code name="oembed_url" type={presence} %}
            </div>
            <div class="col-md-3">
                <button class="btn btn-default" id="oembed-url-check">{_ Try URL _}</button>
                {% javascript %}
                        $('#oembed-url-check').click(function() {
                                var url = $('#{{ #embed_code }}').val();
                                if (url != "") {
                                        z_notify("do_oembed", {
                                                z_delegate: "mod_oembed",
                                                url: url
                                        });
                                }
                                return false;
                        });
                {% endjavascript %}
            </div>
        </div>

        {% if not id %}
             <div class="form-group row">
                <div class="col-md-12">
                    <p>{_ The media title will be automatically detected from its URL. _}</p>
                </div>
            </div>
        {% endif %}

        <div class="form-group row" style="display:none">
            <label class="control-label col-md-3">&nbsp;</label>
            <div class="col-md-9">
                <img id="oembed-image" src="" width="180" />
            </div>
        </div>

        {% if not id %}
            <div class="form-group row">
                <label class="control-label col-md-3" for="oembed-title">{_ Media title _}</label>
                <div class="col-md-9">
                    <input type="text" class="col-lg-4 col-md-4 form-control" id="oembed-title" name="title" value="{{ title|escape }}" {% if not medium.oembed_url %}disabled{% endif %} />
                    {% validate id="oembed-title" name="title" type={presence} %}
                </div>
            </div>

            <div class="form-group row">
                <label class="control-label col-md-3" for="oembed-summary">{_ Summary _}</label>
                <div class="col-md-9">
                    <textarea class="col-lg-4 col-md-4 form-control" id="oembed-summary" name="summary" {% if not medium.oembed_url %}disabled{% endif %}>{{ summary|escape }}</textarea>
                </div>
            </div>
        {% endif %}

        <div class="modal-footer">
            {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
            <button class="btn btn-primary" {% if not medium.oembed_url %}disabled{% endif %} id="oembed-save" type="submit">{% if id %}{_ Replace media item _}{% else %}{_ Make media item _}{% endif %}</button>
        </div>
    </form>
</div>

{% if is_oembed %}
    {% javascript %}
        $('#{{ tab }} a[href="#{{ tab }}-oembed"]').tab('show');
    {% endjavascript %}
{% endif %}

{% endwith %}
{% endwith %}

{% endif %}
