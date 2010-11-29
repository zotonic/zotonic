<div id="logon_form_box">
	<form id="logon_form" method="post" action="postback">
		<h1 class="logon_header">{_ Log on using your _} <span>{{ m.config.site.title.value|default:"Zotonic" }} ID</span></h1>

		<input type="hidden" name="page" value="{{ page|escape }}" />
		<input type="hidden" name="handler" value="username" />

		<div id="logon_username">
			<p class="do_inputoverlay">
				<span>{_ Username _}</span>
				<input type="text" id="username" name="username" value="" autocapitalize="off" autocomplete="on" />
			</p>
		</div>

		<div id="logon_password">
			<p class="do_inputoverlay">
				<span>{_ Password _}</span>
				<input type="password" id="password" name="password" value="" autocomplete="on" />
			</p>
		</div>

		<div id="logon_rememberme" class="rememberme">
			<input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" />
			<label for="{{ #rememberme }}">{_ Stay logged in for two weeks unless I log out. _}</label>
		</div>

		<div class="clearfix"></div>

		<div id="logon_button">
			<button type="submit">{_ Log On _}</button>
		</div>
	</form>
</div>
