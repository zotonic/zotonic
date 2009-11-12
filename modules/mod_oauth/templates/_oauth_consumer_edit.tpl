{% wire id=#form type="submit" postback={consumer_save id=consumer.id} %}
<form id="{{ #form }}" method="POST" action="postback">

    {% tabs id=#tabs %}
    <div id="{{ #tabs }}">
        <ul>
            <li><a href="#{{ #tab }}-details">Details</a></li>
            <li><a href="#{{ #tab }}-auth">Authorization</a></li>
        </ul>

        <div id="{{ #tab }}-details">
            <h2>Application details</h2>
            <div class="form-item">
                <label for="zp-title">Application title:</label><br />
                <input type="text" name="zp-title" id="zp-title" value="{{ consumer.application_title }}" />
            </div>

            <div class="form-item">
                <label for="zp-url">Homepage:</label><br />
                <input type="text" name="zp-url" id="zp-url" value="{{ consumer.application_uri }}" />
            </div>

            <div class="form-item">
                <label for="zp-text">Description:</label><br />
                <textarea name="zp-text" id="zp-text">{{ consumer.application_descr }}</textarea>
            </div>

            <div class="form-item">
                <label for="zp-callback">Callback URL:</label><br />
                <input type="text" name="zp-callback" id="zp-callback" value="{{ consumer.callback_uri }}" />
            </div>

            <div class="form-item clearfix">
                {% if consumer.id %}
                {% button type="submit" text="Update" %}
                {% button action={dialog_close} text="Cancel" %}
                {% else %}
                When done, go to the authorization tab to set permissions.
                {% endif %}
                
            </div>
        </div>
        <div id="{{ #tab }}-auth">

            <h2>Authorization</h2>

            {% with m.oauth_perms.selected[consumer.id] as perms %}
            <table>
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

                {% if consumer.id %}
                {% button type="submit" text="Update" %}
                {% else %}
                {% button type="submit" text="Add application" %}
                {% endif %}
                {% button action={dialog_close} text="Cancel" %}

        </div>
    </div>
</form>
