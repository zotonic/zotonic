{% tabs id=#tabs %}
<div class="tabbable">
    <ul class="nav nav-pills">
	<li class="active"><a data-toggle="tab" href="#{{ #tab }}-media">{_ Media on this page _}</a></li>
	<li><a data-toggle="tab" href="#{{ #tab }}-search">{_ Search other media _}</a></li>
    </ul>

    <div class="tab-content">
        <div class="tab-pane active" id="{{ #tab }}-media">
	    <p>{_ Choose a media item from this page to insert in the body text._}</p>

	    {% with m.rsc[id].o.depiction as ids %}
	    {% include "_choose_media.tpl" %}
	    {% endwith %}

            <div class="modal-footer">
                {% button
                class="btn btn-primary"
                text=_"Add a new media item" 
                action={dialog_media_upload subject_id=id stay
                action={postback 
                postback={zmedia_choose} 
                delegate="action_admin_zmedia_choose"}
	        action={postback
	        postback={reload_media rsc_id=id div_id=media_div_id}
 	        delegate="resource_admin_edit"}
                }
                %}
            </div>

        </div>

        <div class="tab-pane" id="{{ #tab }}-search">
	    <div class="control-group">

                <label for="{{#input}}" class="control-label">{_ Use the autocompleter to search the media in this site. _}</label>
                <div class="controls">
                    <input id="{{#input}}" class="autocompleter span8 do_autofocus" type="text" value="" />
                    <ul id="{{#suggestions}}" class="suggestions-list"></ul>
                </div>
            </div>

            {% wire id=#input
                type="keyup" 
                action={typeselect
                target=#suggestions 
                action_with_id={with_args action={link subject_id=subject_id predicate="depiction" element_id=element_id} arg={object_id select_id}
                }
	        action={postback postback={reload_media rsc_id=id div_id=media_div_id} delegate="resource_admin_edit"}
                action_with_id={with_args action={zmedia_has_chosen} arg={id select_id}}
                action={dialog_close}

                cat=m.predicate.object_category["depiction"]
	        }
            %}
                
	</div>	
    </div>
</div>
