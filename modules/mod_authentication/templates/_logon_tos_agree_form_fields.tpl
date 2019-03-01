<div class="form-group" id="tos_agree_group">
    <div class="checkbox">
        <label for="tos_agree">
            <input type="checkbox" class="disabled" name="tos_agree" id="tos_agree" value="1">

            {_ I agree to the _} <a target="_blank" href="{{ m.rsc.signup_tos.page_url }}">{_ Terms of Service _}</a>
            {_ and the _} <a target="_blank" href="{{ m.rsc.signup_privacy.page_url }}">{_ Privacy policies _}</a>.
        </label>
        {% validate id="tos_agree"
            type={acceptance failure_message=_"You must agree to the Terms in order to continue."}
            message_after="tos_agree_group"
        %}
    </div>
</div>

<div class="form-group">
    <button class="btn btn-primary" type="submit">{_ Continue _}</button>
</div>
