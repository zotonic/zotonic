<image src="http://www.gravatar.com/avatar/{% if comment.user_id %}{{ m.rsc[comment.user_id].email_raw|gravatar_code }}{% else %}{{ comment.gravatar_code }}{% endif %}?s={{ size|default:80|format_integer }}&amp;d=monsterid"
	class="avatar" alt="avatar" width="{{ size|default:80|format_integer }}" height="{{ size|default:80|format_integer }}" />
