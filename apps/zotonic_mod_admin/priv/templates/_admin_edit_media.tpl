{% if medium.mime %}

    {% if medium.media_import %}
    <p class="clear">
        <span class="label label-info">{_ Embed _}</span>
        {% if medium.media_import|match:'^https?:' %}
            <a href="{{ medium.media_import|escape }}">{{ medium.media_import|truncate:80:'…'|escape }}</a>
        {% else %}
            <span class="text-muted">{{ medium.media_import|truncate:80:'…'|escape }}</span>
        {% endif %}
    </p>
    {% endif %}

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

    <div class="admin-edit-media {% if id.is_a.image %}do_cropcenter{% endif %}" id="rsc-image" data-original-width="{{ medium.width }}">
        {% if medium.width < 597 and medium.height < 597 %}
            {% media medium mediaclass="admin-media-cropcenter" %}
        {% else %}
            {% media medium mediaclass="admin-media" %}
        {% endif %}
    </div>

    <div class="form-group clearfix">
        {% if id.is_a.image %}
            <input type="hidden" name="crop_center" id="crop_center" value="{{ id.crop_center }}" />
            <a href="#" id="crop-center-remove" class="btn btn-default">
                <i class="glyphicon glyphicon-remove"></i> {_ Remove crop center _}
            </a>
            <span id="crop-center-message" class="text-muted">{_ Click the image to set the cropping center. _}</span>
        {% endif %}

        <p class="text-right">
            {% if medium.size > 0 %}
                <a target="_blank" class="btn btn-default" href="{% url media_inline id=id %}" class="button">{_ View _}</a>
                <a target="_blank" class="btn btn-default" href="{% url media_attachment id=id %}" class="button">{_ Download _}</a>
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
                text=_"Replace this media item"
                class="btn btn-primary"
                element="a"
        	    action={dialog_media_upload
                    id=id
                    action={update
                        target="media-edit-view"
                        template="_admin_edit_media_all.tpl"
                        id=id
                    }
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
            <p class="text-danger text-right">
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
            {% button text=medium|if:_"Replace this media item":_"Add media item"
                class="btn btn-primary"
                element="a"
    	        action={dialog_media_upload
    	            id=id
    	            action={update
    	                target="media-edit-view"
    	                template="_admin_edit_media_all.tpl"
    	                id=id
    	            }
    	            center=0
    	        }
    	        disabled=not id.is_editable
    	    %}
        </div>
    </div>
{% endif %}
