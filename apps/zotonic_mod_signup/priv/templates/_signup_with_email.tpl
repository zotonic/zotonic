<div id="signup-email" class="signup signup-email">
    {# Step 1:
     # - Request email address
     #}
    <div id="signup-email-step1">
        {% include "_signup_with_email_step1.tpl" %}
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
