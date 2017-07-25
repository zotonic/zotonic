{#
Params:
show_signup_tos_title
show_signup_tos_info
show_signup_tos_checkbox
#}

{% if show_signup_tos_title %}
    <h3>{_ Check our Terms of Service and Privacy policies _}</h3>
{% endif %}

{% if show_signup_tos_info %}
    <p>{_ We will be very careful with all the information given to us and will never give your name or address away without your permission. We do have some rules that we need you to agree with. _}</p>
    <p id="signup_error_tos_agree" class="alert alert-danger">{_ To sign up you must agree with the Terms of Service and Privacy policies. _}</p>
{% endif %}

{% if show_signup_tos_checkbox %}
    <div class="form-group" id="signup_tos">
        <div class="checkbox" id="signup_tos_agree_group">
            <label for="signup_tos_agree">
                <input type="checkbox" name="signup_tos_agree" id="signup_tos_agree" value="1" />
                {_ I agree to the _} <a target="_blank" href="{{ m.rsc.signup_tos.page_url }}">{_ Terms of Service _}</a>
                {_ and the _} <a target="_blank" href="{{ m.rsc.signup_privacy.page_url }}">{_ Privacy policies _}</a>.
            </label>
            {% validate id="signup_tos_agree"
                type={acceptance failure_message=_"You must agree to the Terms in order to sign up."}
                message_after="signup_tos_agree_group"
            %}
        </div>
    </div>
{% endif %}
