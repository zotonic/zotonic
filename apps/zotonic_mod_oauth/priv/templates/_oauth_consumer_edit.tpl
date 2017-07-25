{% wire id=#form type="submit" postback={consumer_save id=consumer.id} %}
<form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">

    <div class="tabbable">

        <ul class="nav nav-pills">
            <li class="active"><a data-toggle="pill" href="#{{ #tab }}-details">{_ Details _}</a></li>
            <li><a data-toggle="pill" href="#{{ #tab }}-auth">{_ Authorization _}</a></li>
        </ul>

        <div class="tab-content">
            <div class="tab-pane active" id="{{ #tab }}-details">

                {% if consumer %}
                    <div class="form-group row">
                        <label for="zp-ckey" class="control-label col-md-3">{_ Consumer key _}</label>
                        <div class="col-md-9">
                            <input class="form-control" type="text" id="zp-ckey" readonly value="{{ consumer.consumer_key }}" />
                        </div>
                    </div>

                    <div class="form-group row">
                        <label for="zp-csec" class="control-label col-md-3">{_ Consumer secret _}</label>
                        <div class="col-md-9">
                            <input class="form-control" type="text" id="zp-csec" readonly value="{{ consumer.consumer_secret }}" />
                        </div>
                    </div>
                {% endif %}

                <div class="form-group row">
                    <label for="zp-title" class="control-label col-md-3">{_ Application title _}</label>
                    <div class="col-md-9">
                        <input class="form-control" type="text" name="zp-title" id="zp-title" value="{{ consumer.application_title|escape }}" />
                    </div>
                </div>

                <div class="form-group row">
                    <label for="zp-url" class="control-label col-md-3">{_ Homepage _}</label>
                    <div class="col-md-9">
                        <input class="form-control" type="text" name="zp-url" id="zp-url" value="{{ consumer.application_uri|escape }}" />
                    </div>
                </div>

                <div class="form-group row">
                    <label for="zp-text" class="control-label col-md-3">{_ Description _}</label>
                    <div class="col-md-9">
                        <textarea class="form-control" name="zp-text" id="zp-text">{{ consumer.application_descr|escape }}</textarea>
                    </div>
                </div>

                <div class="form-group row">
                    <label for="zp-callback" class="control-label col-md-3">{_ Callback URL _}</label>
                    <div class="col-md-9">
                        <input class="form-control" type="text" name="zp-callback" id="zp-callback" value="{{ consumer.callback_uri|escape }}" />
                    </div>
                </div>

                <div class="modal-footer">
                    {% if consumer.id %}
                        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
                        {% button class="btn btn-primary" type="submit" text=_"Update" %}
                    {% else %}
                        <a href="#" data-link="tab-auth" class="btn btn-primary">{_ Next step _}</a>
                        {% javascript %}
                            $("a[data-link='tab-auth']").on("click", function() {
                                $('a[href="#{{ #tab }}-auth"]').tab('show');
                            });
                        {% endjavascript %}
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
                {% button class="btn btn-default" action={dialog_close} text=_"Cancel" %}
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
