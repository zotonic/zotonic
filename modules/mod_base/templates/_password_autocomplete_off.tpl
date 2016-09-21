{#
    Fake usernames/password fields to stop Safari from autofilling
    See https://github.com/zotonic/zotonic/issues/811
#}
<div style="position:absolute;height:0px; overflow:hidden;">
    <input type="text" tab-index="-1" id="fake-username" name="fake-username" class="nosubmit" value="" />
    <input type="password" tab-index="-1" id="fake-password" name="fake-password" class="nosubmit" value="" />
</div>
