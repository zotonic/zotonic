{% wire id=#form type="submit" postback={consumer_save id=consumer.id} %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">

    <div class="tabbable">

        <ul class="nav nav-pills">
            <li class="active"><a data-toggle="pill" href="#{{ #tab }}-details">{_ Details _}</a></li>
            <li><a data-toggle="pill" href="#{{ #tab }}-auth">{_ Authorization _}</a></li>
        </ul>

        <div class="tab-content">
            <div class="tab-pane active" id="{{ #tab }}-details">

                <div class="control-group">
                    <label for="zp-title" class="control-label">{_ Application title _}:</label>
                    <div class="controls">
                        <input type="text" name="zp-title" id="zp-title" value="{{ consumer.application_title }}" />
                    </div>
                </div>

                <div class="control-group">
                    <label for="zp-url" class="control-label">{_ Homepage _}:</label>
                    <div class="controls">
                        <input type="text" name="zp-url" id="zp-url" value="{{ consumer.application_uri }}" />
                    </div>
                </div>

                <div class="control-group">
                    <label for="zp-text" class="control-label">{_ Description _}:</label>
                    <div class="controls">
                        <textarea name="zp-text" id="zp-text">{{ consumer.application_descr }}</textarea>
                    </div>
                </div>

                <div class="control-group">
                    <label for="zp-callback" class="control-label">{_ Callback URL _}:</label>
                    <div class="controls">
                        <input type="text" name="zp-callback" id="zp-callback" value="{{ consumer.callback_uri }}" />
                    </div>
                </div>

                <div class="modal-footer">
                    {% if consumer.id %}
                    {% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}
                    {% button class="btn btn-primary" type="submit" text=_"Update" %}
                    {% else %}
                    {_ When done, go to the authorization tab to set permissions. _}
                    {% endif %}
                </div>
            </div>

            <div class="tab-pane" id="{{ #tab }}-auth">

                <p>{_ Allow users of this application access to the following API calls: _}</p>
                
                {% with m.oauth_perms.selected[consumer.id] as perms %}
                <table class="table" >
                    {% for perm in m.oauth_perms %}
                    <tr>
                        <td>
                            <input type="checkbox" name="zp-perm" value="{{ perm.value }}" id="perm-{{ perm.value }}"
                                   {% for p in perms %}{% ifequal perm.value p.perm %}checked="checked"{% endifequal %}{% endfor %}
                                />
                        </td>
                        <td><label for="perm-{{ perm.value }}"><tt>{{ perm.value }}</tt></label></td>
                        <td>{{ perm.title }}</td>
                    </tr>
                    {% endfor %}
                </table>
                {% endwith %}

                <div class="modal-footer">
                    {% button class="btn" action={dialog_close} text=_"Cancel" %}
                    {% if consumer.id %}
                    {% button class="btn btn-primary" type="submit" text=_"Update" %}
                    {% else %}
                    {% button class="btn btn-primary" type="submit" text=_"Add application" %}
                    {% endif %}
                </div>
            </div>
        </div>
    </div>
        
</form>
