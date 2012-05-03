<div class="item-wrapper">
	<h3 class="above-item">Google</h3>
	<div class="item">
		<fieldset class="admin-form">
			<div class="form-item clearfix">
				<label for="seo_google-analytics">{_ Google Analytics tracking id.<br/>
					You find this id in the tracking script, it has the format <strong>UA-123456-1</strong>. 
					<a href="http://www.google.com/support/googleanalytics/bin/answer.py?hl=en&amp;answer=55603" title="Google Analytics Help">Where can I find my tracking script?</a> _}
				</label>
				<input type="text" id="seo_google-analytics" name="seo_google-analytics" value="{{ m.config.seo_google.analytics.value|escape }}" />
			</div>

			<div class="form-item clearfix">
				<label for="seo_google-webmaster">{_ Google Webmaster Tools<br/>
					Enter here the verification code for <a href="https://www.google.com/webmasters/tools">Google Webmaster Tools</a>. Copy the value of the content attribute in the meta tag provided by Google. _}
				</label>
				<input type="text" id="seo_google-webmaster_verify" name="seo_google-webmaster_verify" value="{{ m.config.seo_google.webmaster_verify.value|escape }}" />
			</div>
		</fieldset>
	</div>
</div>
