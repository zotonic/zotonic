{% if reason == "Language mismatch" %}{_ Their preferred language does not match your selection. _}
{% elseif reason == "Missing translation" %}{_ This page is not available in their language. _}
{% elseif reason == "Unknown recipient language" %}{_ Their language preference is not recognized. _}
{% elseif reason == "No preferred language" %}{_ They have not set a preferred language. _}
{% elseif reason == "Previously sent or already pending" %}{_ They have already received this version, or it is already scheduled for them. _}
{% elseif reason == "No previous failure" %}{_ There is no failed delivery to retry. _}
{% elseif reason == "Previous failure; use retry failed recipients" %}{_ A previous delivery failed. Choose retry failed recipients to try again. _}
{% elseif reason == "Address blocked or suppressed" %}{_ Sending to this address is blocked. _}
{% elseif reason == "Invalid email address" %}{_ The email address is invalid. _}
{% elseif reason == "Missing email address" %}{_ No email address is available. _}
{% else %}{{ reason|escape }}{% endif %}
