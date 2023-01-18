<p>
    {_ Create a new person to link the answer to. _}
</p>

{% wire id=#form
        type="submit"
        postback={link_person_new survey_id=id answer_id=answer_id}
        delegate=`survey_admin`
%}
<form id="{{ #form }}" action="postback">
    <fieldset>
        <div class="row">
            <div class="form-group col-lg-5 col-md-5 label-floating">
                <input class="form-control" id="name_first" type="text" name="name_first" value="{{ person.name_first|escape }}" placeholder="{_ First _}">
                <label class="control-label" for="name_first">{_ First _}</label>
            </div>
            <div class="form-group col-lg-2 col-md-2 label-floating">
                <input class="form-control" id="name_surname_prefix" type="text" name="name_surname_prefix" value="{{ person.name_surname_prefix|escape }}" placeholder="{_ Sur. prefix _}">
                <label class="control-label" for="name_surname_prefix">{_ Sur. prefix _}</label>
            </div>
            <div class="form-group col-lg-5 col-md-5 label-floating">
                <input class="form-control" id="name_surname" type="text" name="name_surname" value="{{ person.name_surname|escape }}" placeholder="{_ Surname _}">
                <label class="control-label" for="name_surname">{_ Surname _}</label>
            </div>
        </div>
    </fieldset>

    <div class="form-group label-floating">
            <input class="form-control" id="phone" type="text" name="email" inputmode="email" value="{{ person.email|escape }}" placeholder="{_ Email _}">
        <label class="control-label" for="phone">{_ Email _}</label>
    </div>

    <div class="form-group label-floating">
            <input class="form-control" id="phone" type="text" name="phone" inputmode="tel" value="{{ person.phone|escape }}" placeholder="{_ Telephone _}">
        <label class="control-label" for="phone">{_ Telephone _}</label>
    </div>

    <div class="form-group label-floating">
        {% if m.modules.active.mod_l10n %}
            <select class="form-control" id="address_country" name="address_country">
                <option value=""></option>
                {% optional include "_l10n_country_options.tpl" country=person.address_country|escape %}
            </select>
        {% else %}
            <input class="form-control" id="address_country" type="text" name="address_country" value="{{ person.address_country|escape }}" placeholder="{_ Country _}">
        {% endif %}
        <label class="control-label" for="address_country">{_ Country _}</label>
    </div>
    {% wire id="address_country"
            type="change"
            action={script script="
                if ($(this).val() != '') $('#visit_address').slideDown();
                else $('#visit_address').slideUp();
            "}
    %}
    <div id="visit_address" {% if not person.address_country %}style="display:none"{% endif %}>
        <div class="form-group label-floating">
            <input class="form-control" id="address_street_1" type="text" name="address_street_1" value="{{ person.address_street_1|escape }}" placeholder="{_ Street Line 1 _}">
            <label class="control-label" for="address_street_1">{_ Street Line 1 _}</label>
        </div>

        <div class="form-group label-floating">
            <input class="form-control" id="address_street_2" type="text" name="address_street_2" value="{{ person.address_street_2|escape }}" placeholder="{_ Street Line 2 _}">
            <label class="control-label" for="address_street_2">{_ Street Line 2 _}</label>
        </div>

        <div class="row">
            <div class="form-group col-lg-6 col-md-6 label-floating">
                <input class="form-control" id="address_city" type="text" name="address_city" value="{{ person.address_city|escape }}" placeholder="{_ City _}">
                <label class="control-label" for="address_city">{_ City _}</label>
            </div>

            <div class="form-group col-lg-6 col-md-6 label-floating">
                <input class="form-control" id="address_postcode" type="text" name="address_postcode" value="{{ person.address_postcode|escape }}" placeholder="{_ Postcode _}">
                <label class="control-label" for="address_postcode">{_ Postcode _}</label>
            </div>
        </div>

        <div class="row">
            <div class="form-group col-lg-6 col-md-6 label-floating">
                <input class="form-control" id="address_state" type="text" name="address_state" value="{{ person.address_state|escape }}" placeholder="{_ State _}">
                <label class="control-label" for="address_state">{_ State _}</label>
            </div>
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
        <button type="submit" class="btn btn-primary">
            {_ Create and link _}
        </button>
    </div>
</form>
