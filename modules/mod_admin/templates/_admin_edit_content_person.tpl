{% extends "admin_edit_widget_std.tpl" %}

{# Show the edit fields to edit the name of a person #}

{% block widget_title %}{{ _"Person name"|escapejs }}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}content-person{% endblock %}

{% block widget_content %}

    {% with m.rsc[id] as r %}
        <div class="row">
            <div class="col-lg-4 col-md-4">
    	        <label for="name_first">{_ First _}</label>
            </div>
            <div class="col-lg-2 col-md-2">
                <label for="name_middle">{_ Middle _}</label>
            </div>
            <div class="col-lg-2 col-md-2">
                <label for="name_surname_prefix">{_ Sur. prefix _}</label>
            </div>
            <div class="col-lg-4 col-md-4">
                <span class="pull-right">
                    <a href="javascript:void(0)" class="btn btn-primary btn-xs do_dialog" data-dialog="title: '{{ _"Help about person name."|escapejs }}', text: '{{ _"Here you can edit the person's name.  Use the title of the base content for the display name of this person."|escapejs }}<br/><br/>{{ _"<strong>First</strong> also known as given name, forename or Christen name.<br/><strong>Middle</strong> often shortened to an initial like in <em>John D. Rockefeller</em>.<br/><strong>Surname prefix</strong> like the Dutch <em>van, van de, der</em>.<br/><strong>Surname</strong> also known as family name or last name."|escapejs }}'" title="{_ Need more help? _}"><i class="glyphicon glyphicon-question-sign"></i></a>
            </span>

            <label for="name_surname">{_ Surname _}</label>
        </div>
    </div>
    <div class="form-group row">
        <div class="col-lg-4 col-md-4">
            <input class="form-control" id="name_first" type="text" name="name_first" value="{{ r.name_first }}" /> 
        </div>
        <div class="col-lg-2 col-md-2">
	        <input class="form-control" id="name_middle" type="text" name="name_middle" value="{{ r.name_middle }}" />
        </div>
        <div class="col-lg-2 col-md-2">
	        <input class="form-control" id="name_surname_prefix" type="text" name="name_surname_prefix" value="{{ r.name_surname_prefix }}" />
        </div>

        <div class="col-lg-4 col-md-4">
	        <input class="form-control" id="name_surname" type="text" name="name_surname" value="{{ r.name_surname }}" />
        </div>
    </div>
{% endwith %}
{% endblock %}
