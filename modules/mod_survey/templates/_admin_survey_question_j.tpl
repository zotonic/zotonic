<li class="page-jump" id="{{ #j }}">
	{_ if _}   <input name="jump-condition" class="input-medium jump-condition {% if nosubmit %}nosubmit{% endif %}" type="text" placeholder="{_ question == 1 _}" value="{{ condition }}" />
	{_ goto _} <input name="jump-target" class="input-small jump-target {% if nosubmit %}nosubmit{% endif %}" type="text" placeholder="{_ question _}" value="{{ target }}" />
	
	<ul class="nav nav-pills">
		<li><a href="#jump-go">{_ Go to question _}</a></li>
		<li><a href="#jump-delete">{_ Delete page jump _}</a></li>
	</ul>
</li>
