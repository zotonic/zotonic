<div id="signup-email" class="signup signup-email">
    {# Step 1:
     # - Request email address
     #}
    <div id="signup-email-step1">
        {% wire id="signup_form"
                type="submit"
                postback={signup_email_step1 page=q.p props=props signup_props=signup_props}
                delegate=`controller_signup`
        %}
        <form id="signup_form" method="post" action="postback">
            <div class="form-group">
                {% if props.email %}
                    <p class="form-control-prefilled">
                        <span>{_ Your email address: _}</span> {{ props.email|escape }}
                    </p>
                {% else %}
                    <input id="signup-email-input" class="form-control" autofocus type="email" name="email" autocomplete="email" placeholder="you@example.com" required>
                    {% validate id="signup-email-input" name="email" type={email} %}
                {% endif %}
            </div>
            <div class="form-group">
                <button class="btn btn-primary" type="submit">{_ Next _}</button>
            </div>
            <p class="help-block">
                {_ We will mail you a code to confirm your email address. _}
            </p>
        </form>
    </div>

    {# Step 2:
     # - Check if account exists
     # - Check if email address blocked
     # - Check if email domain is managed by a service provider
     # - Check on rate limit for email address
     #
     # If any failed:
     # - Show an error
     #
     # If all checked:
     # - Mail a code to the email address
     # - Show form to enter code
     # - Show status of email sending
     #
     # Extra navigation:
     # - Button to hide this and show step1
     #}
     <div id="signup-email-step2" style="display: none">
     </div>

     {# Step 3:
      #
      # Code was entered correctly, show form with:
      # - Name fields (first, prefix, last)
      # - Optional: Username (prefilled with email address or name part of email address)
      # - Password
      # - Agree to T and C checkbox
      # - Sign up button
      #
      # After success:
      # - Create one-time logon code
      # - Let client side auth model logon and redirect
      #}
    <div id="signup-email-step3" style="display: none">
    </div>
</div>
