
<p>
    {_ Find a person to link the answer to. If you do not find a person then you can create a new person. _}
</p>

{% wire id=#form
        type="submit"
        postback={link_person survey_id=id answer_id=answer_id}
        delegate=`survey_admin`
%}
<form id="{{ #form }}" action="postback">

    {% if m.acl.use.mod_admin_identity %}
        {% if answers['email']|default:answers['Email'] as em %}
            {% if em.answer %}
                {% if m.identity.lookup.email[em.answer] as idns %}
                    <h3>{_ Matching email address _}</h3>

                    <table class="table table-striped">
                        {% for idn in idns %}
                        {% with idn.rsc_id as id %}
                            <tr>
                                <td>
                                    <a href="{% url admin_edit_rsc id=id %}" target=_"blank">
                                        {% include "_name.tpl" %} <i class="glyphicon glyphicon-new-window"></i>
                                    </a><br>
                                    <span class="text-muted">
                                        {{ id.category_id.title }}
                                        {% if m.identity[id].is_user %}
                                            ({_ member _})
                                        {% endif %}
                                        &ndash; <small>{{ id.modified|date:_"Y-m-d" }}</small>
                                    </span>
                                </td>
                                <td>
                                    {% if id.address_country %}
                                        {{ id.address_street_1 }}
                                        {% if id.address_street_2 %}
                                            {{ id.address_street_2 }}<br>
                                        {% endif %}
                                        {{ id.address_city }}<br>
                                        {{ m.l10n.country_name[id.address_country] }}
                                    {% endif %}
                                </td>
                                <td>
                                    <button type="submit" class="btn btn-default pull-right" value="{{ id }}" name="id">
                                        {_ Select _}
                                    </button>
                                </td>
                            </tr>
                        {% endwith %}
                        {% endfor %}
                    </table>
                {% endif %}
            {% endif %}
        {% endif %}
    {% endif %}

    <h3>{_ Matching text _}</h3>

    {% with [
        answers['name']['answer'],
        answers['name_first']['answer'],
        answers['name_surname']['answer'],
        answers['address_street_1']['answer'],
        answers['street_1']['answer'],
        answers['address_street']['answer'],
        answers['street']['answer']
        ]|join:' ' as text %}

        <table class="table table-striped">
            {% for id in m.search::%{
                    text: text
                    cat: `person`
                    pagelen: 100
                }
            %}
                <tr>
                    <td>
                        <a href="{% url admin_edit_rsc id=id %}" target=_"blank">
                            {% include "_name.tpl" %} <i class="glyphicon glyphicon-new-window"></i>
                        </a><br>
                        <span class="text-muted">
                            {{ id.category_id.title }}
                            {% if m.identity[id].is_user %}
                                ({_ member _})
                            {% endif %}
                            &ndash; <small>{{ id.modified|date:_"Y-m-d" }}</small>
                        </span>
                    </td>
                    <td>
                        {% if id.address_country %}
                            {{ id.address_street_1 }}
                            {% if id.address_street_2 %}
                                {{ id.address_street_2 }}<br>
                            {% endif %}
                            {{ id.address_city }}<br>
                            {{ m.l10n.country_name[id.address_country] }}
                        {% endif %}
                    </td>
                    <td>
                        <button type="submit" class="btn btn-default pull-right" value="{{ id }}" name="id">
                            {_ Select _}
                        </button>
                    </td>
                </tr>
            {% endfor %}
        </table>
    {% endwith %}
</form>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
    {% button class="btn btn-primary" text=_"Create new person"
              postback={link_new_person survey_id=id answer_id=answer_id}
              delegate=`survey_admin`
    %}
</div>

