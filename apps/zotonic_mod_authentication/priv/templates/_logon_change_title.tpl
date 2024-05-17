{#
Params:
- username: set by controller_logon
#}
<h2 class="z-logon-title">{_ Change your password _}</h2>

<p>{% trans "Enter the new password for your account <strong>{username}</strong>."
            username=m.identity[m.acl.user].username|escape
%}</p>
