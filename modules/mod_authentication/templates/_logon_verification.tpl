<div id="logon_verification_pending">
	<h1 class="logon_header">{_ Your account needs verification _}</h1>

	<p>{_ Click on the button below to e-mail you a verification message. _}</p>

	<form id="logon_verification_form" method="POST" action="postback">
		<input id="logon_verification_user_id" type="hidden" name="user_id" value="" />
		<button type="submit">{_ Send Verification Message _}</button>
	</form>
</div>

<div id="logon_verification_sent">
	<h1 class="logon_header">{_ Check your e-mail! _}</h1>

	<p>{_ We have sent you an e-mail. In the e-mail you will find instructions on how to confirm your account. _}</p>

	<p>{_ When you don’t receive the e-mail within a few minutes then be sure to check your spam filter and spam folders. _}</p>
</div>
