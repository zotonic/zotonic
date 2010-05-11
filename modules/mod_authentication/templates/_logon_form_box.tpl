<div id="logon_form_box">
	<form id="logon_form" method="post" action="postback">
		<h1 class="logon_header">Log on using your <span>{{ m.config.site.title.value|default:"Zotonic" }} ID</span></h1>

		<input type="hidden" name="page" value="{{ page|escape }}" />
		<input type="hidden" name="handler" value="username" />

		<div id="logon_username">
			<p class="do_inputoverlay">
				<span>Username</span>
				<input type="text" id="username" name="username" value="" autocapitalize="off" autocomplete="on" />
			</p>
		</div>

		<div id="logon_password">
			<p class="do_inputoverlay">
				<span>Password</span>
				<input type="password" id="password" name="password" value="" autocomplete="on" />
			</p>
		</div>

		<div id="logon_rememberme" class="rememberme">
			<input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" />
			<label for="{{ #rememberme }}">Remember me for two weeks</label>
		</div>

		<div class="clearfix"></div>

		<div id="logon_button">
			<button>Log On</button>
		</div>
	</form>
</div>
