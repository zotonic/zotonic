{% if medium.mime %}
    <p>
        {{ medium.mime }} 
        {% if medium.filename %}
            {% if medium.width and medium.height %}
            &ndash; {{ medium.width }} x {{ medium.height }} {_ pixels _}
            {% endif %}
            {% if medium.size %}
                &ndash; {{ medium.size|filesizeformat }}
            {% endif %}
            &ndash; {{ medium.filename }}
        {% endif %}
        &ndash; {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
    </p>

    {% if medium.width and medium.height %}
    <div class="admin-edit-media {% if id.is_a.image %}do_cropcenter{% endif %}" id="rsc-image" data-original-width="{{ medium.width }}">
        {% if medium.width < 597 and medium.height < 597 %}
            {% media medium mediaclass="admin-media-cropcenter" %}
        {% else %}
            {% media medium mediaclass="admin-media" %}
        {% endif %}   
    </div>
    {% endif %}

    <div class="form-group clearfix">
        {% if id.is_a.image %}
            <input type="hidden" name="crop_center" id="crop_center" value="{{ id.crop_center }}" />
            <a href="#" id="crop-center-remove" class="btn btn-default">
                <i class="glyphicon glyphicon-remove"></i> {_ Remove crop center _}
            </a>
            <span id="crop-center-message">{_ Click the image to set the cropping center. _}</span>
        {% endif %}

        <div class="pull-right">
            <a class="btn btn-default" href="{% url media_attachment star=medium.filename %}" class="button">{_ Download _}</a>
            {% button   text=_"Replace this media item"
                class="btn btn-primary"
    	    action={dialog_media_upload id=id action={update target="media-edit-view" template="_admin_edit_media_all.tpl" id=id}} 
    	    disabled=not id.is_editable %}

        </div>
    </div>
{% else %}
    <p>
        {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
    </p>
    <div class="form-group clearfix">
        <div class="pull-right">
            {% button   text=_"Replace this media item"
                class="btn btn-primary"
    	    action={dialog_media_upload id=id action={update target="media-edit-view" template="_admin_edit_media_all.tpl" id=id}} 
    	    disabled=not id.is_editable %}

        </div>
    </div>
{% endif %}
