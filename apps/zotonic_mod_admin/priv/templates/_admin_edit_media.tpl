{% if medium.mime %}

    {% if medium.media_import %}
    <p class="clear">
        <span class="label label-info">{_ Embed _}</span>
        {% if medium.media_import|match:'^https?:' %}
            <a href="{{ medium.media_import|sanitize_url|escape }}" target="_blank" rel="noopener noreferrer">{{ medium.media_import|truncate:80:'…'|escape }}</a>
        {% else %}
            <span class="text-muted">{{ medium.media_import|truncate:80:'…'|escape }}</span>
        {% endif %}
    </p>
    {% endif %}

    <p>
        {{ medium.mime }}
        {% if medium.width and medium.height %}
            <span class="text-muted">&ndash;</span> {{ medium.width }} x {{ medium.height }} {_ pixels _}
        {% endif %}
        {% if medium.duration %}
            <span class="text-muted">&ndash;</span> {{ medium.duration|format_duration }}
        {% endif %}
        {% if medium.size %}
            <span class="text-muted">&ndash;</span> {{ medium.size|filesizeformat }}
        {% endif %}
        <span class="text-muted">
            {% if medium.filename %}
                &ndash; {{ medium.filename }}
            {% endif %}
            &ndash; {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
        </span>
    </p>

    <hr>

    <div class="admin-edit-media" id="rsc-image" data-original-width="{{ medium.width }}">
        {% if medium.width < 597 and medium.height < 597 %}
            {% media medium mediaclass="admin-media-cropcenter" %}
        {% else %}
            {% media medium mediaclass="admin-media" %}
        {% endif %}
    </div>

    <hr>

    {% include "_edit_medium_language.tpl" %}

    <div class="form-group clearfix">
        <p class="text-right">
            {% all include "_admin_edit_media_button.tpl" %}

            {% if medium.size > 0 %}
                <a target="_blank" class="btn btn-default" href="{% url media_inline id=id %}" class="button">{_ View _}</a>
                <a target="_blank" class="btn btn-default" href="{% url media_attachment id=id %}" class="button"download>{_ Download _}</a>
                <input type="text" style="position: absolute; top:0; left:-9999px;" id="url-media-download" value="{% url media_attachment id=id absolute_url %}">
                {% button
                    text=_"Copy download link"
                    class="btn btn-default"
                    action={script
                        script=["
                            var copyText = document.getElementById('url-media-download');
                            copyText.select();
                            document.execCommand('copy');
                            z_growl_add('", _"Copied download link to clipboard" ,"');
                        "]
                    }
                %}
            {% endif %}
            {% button
                text=_"Delete"
                class="btn btn-danger"
                element="a"
                action={confirm
                    text=[
                        _"Are you sure you want to delete the media from this page?",
                        "<br><br>",
                        _"The page will stay, but without the media item."
                    ]
                    ok=_"Delete"
                    is_danger
                    postback={delete_media
                        id=id
                    }
                    delegate=`controller_admin_edit`
                }
                disabled=not (id.is_editable and medium)
            %}
            {% button
                text=_"Replace"
                class="btn btn-primary"
                element="a"
        	    action={dialog_media_upload
                    id=id
                    intent="update"
    	            center=0
    	        }
    	        disabled=not id.is_editable
    	    %}
        </p>
        {% if medium.is_av_sizelimit %}
            <p class="text-danger text-right">
                <span class="glyphicon glyphicon-alert"></span> {_ This file has not been scanned for viruses because it is too large. _}
            </p>
        {% elseif medium.is_av_scanned %}
            <p class="text-muted text-right">
                <span class="glyphicon glyphicon-ok-sign"></span> {_ This file has been scanned for viruses. _}
            </p>
        {% elseif m.modules.active.mod_clamav %}
            <p class="text-muted text-right">
                <span class="glyphicon glyphicon-alert"></span> {_ This file has not been scanned for viruses. _}
            </p>
        {% endif %}
    </div>
{% else %}
    {% if medium.created %}
        <p>
            {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
        </p>
    {% endif %}
    <div class="form-group clearfix">
        <div class="pull-right">
            {% button text=medium|if:_"Replace":_"Add media content"
                class="btn btn-primary"
                element="a"
    	        action={dialog_media_upload
    	            id=id
                    intent="update"
    	            center=0
    	        }
    	        disabled=not id.is_editable
    	    %}
        </div>
    </div>
{% endif %}
