<div class="social-icons">

    {% with "#55acee" as brand_color %}
        <a href="https://twitter.com/share?text={{ id.title|urlencode }}&amp;lang={{ z_language }}" onclick="return !window.open(this.href, 'Twitter', 'width=600,height=300,location=0,toolbar=0,scrollbars=0,status=0')" title="Twitter" class="btn btn-social-icon" style="background-color: {{ brand_color }}; color: #fff;">
            <span class="z-icon z-icon-twitter" ></span>
        </a>
    {% endwith %}

    {% with "#3b5998" as brand_color %}
        <a href="http://www.facebook.com/sharer.php?u=http%3A%2F%2F{{ m.site.hostname }}{{ id.page_url|urlencode }}&amp;t={{ id.title|urlencode }}" onclick="return !window.open(this.href, 'Facebook', 'width=700,height=400,toolbar=0,location=0,scrollbars=0,status=0')" title="Facebook" class="btn btn-social-icon" style="background-color: {{ brand_color }}; color: #fff;">
            <span class="z-icon z-icon-facebook" ></span>
        </a>
    {% endwith %}

    {% all include "_share_page_option.tpl" id=id %}

</div>
<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} %}
</div>
