{%
	wire id="logon_confirm_form"
	type="submit"
	postback={logon_confirm on_success=on_success on_success={dialog_close}}
	delegate="controller_logon"
%}
<form id="logon_confirm_form" method="post" action="postback">
	<h1 class="z-logon-title">{_ Please confirm your _} <span>{{ m.config.site.title.value|default:"Zotonic" }} password</span></h1>

	<input type="hidden" name="page" value="{{ page|escape }}" />
	<input type="hidden" name="handler" value="username" />

	<div id="logon_password">
		<p class="do_inputoverlay">
			<span>{_ Password _}</span>
			<input class="form-control" type="password" id="password" name="password" value="" autocomplete="off" />
		</p>
	</div>

	<p class="error" id="logon_confirm_error" style="display:none"> {_ Confirmation failure _} </p>

	<div class="clearfix"></div>

	<div id="confirm_button">
		<button type="submit">{_ Confirm _}</button>
	</div>
	<div id="cancel_button">
	    {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
	</div>
</form>

