{% wire id=#form type="submit" postback={consumer_save id=consumer.id} %}
<form id="{{ #form }}" method="POST" action="postback">

    {% tabs id=#tabs %}
    <div id="{{ #tabs }}">
        <ul class="clearfix">
            <li><a href="#{{ #tab }}-details">{_ Details _}</a></li>
            <li><a href="#{{ #tab }}-auth">{_ Authorization _}</a></li>
        </ul>

        <div id="{{ #tab }}-details">

            <div class="form-item clearfix">
                <label>{_ Application title _}:
                    <input type="text" name="zp-title" id="zp-title" value="{{ consumer.application_title }}" />
                </label>
            </div>

            <div class="form-item clearfix">
                <label>{_ Homepage _}:
                    <input type="text" name="zp-url" id="zp-url" value="{{ consumer.application_uri }}" />
                </label>
            </div>

            <div class="form-item clearfix">
                <label>{_ Description _}:
                    <textarea name="zp-text" id="zp-text">{{ consumer.application_descr }}</textarea>
                </label>
            </div>

            <div class="form-item clearfix">
                <label>{_ Callback URL _}:
                    <input type="text" name="zp-callback" id="zp-callback" value="{{ consumer.callback_uri }}" />
                </label>
            </div>

            <div class="form-item clearfix">
                {% if consumer.id %}
                {% button type="submit" text="Update" %}
                {% button action={dialog_close} text="Cancel" %}
                {% else %}
                {_ When done, go to the authorization tab to set permissions. _}
                {% endif %}
                
            </div>
        </div>
        <div id="{{ #tab }}-auth">

            {% with m.oauth_perms.selected[consumer.id] as perms %}
            <table width="100%" border="0">
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

            <div class="form-item clearfix">
                {% if consumer.id %}
                {% button type="submit" text=_"Update" %}
                {% else %}
                {% button type="submit" text=_"Add application" %}
                {% endif %}
                {% button action={dialog_close} text=_"Cancel" %}
            </div>
        </div>
    </div>
</form>
