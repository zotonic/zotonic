<div class="widget">
    <a href="https://letsencrypt.org/" target="_blank">
        <img src="/lib/images/letsencrypt-logo-horizontal.png"
             alt="{_ Let’s Encrypt Certificate _}"
             style="height: 40px; margin: 2px 10px 0 0;"
             class="pull-right">
    </a>

    <div class="widget-header">
        {{ nr }}. {_ Let’s Encrypt Certificate _}
    </div>
    <div class="widget-content">
        <p>{_ Let’s Encrypt provides free SSL certificates for websites. Here you can request such a certificate. _}</p>

        <p><a href="https://letsencrypt.org/" target="_blank">{_ Read more at the Let’s Encrypt website _} &raquo;</a></p>

        {% live template="_ssl_letsencrypt_status.tpl"
                topic="bridge/origin/model/letsencrypt/event/status"
        %}

        <p class="help-block">
            <i class="glyphicon glyphicon-info-sign"></i> {_ If there are problems requesting a Let’s Encrypt certificate then check: _}
            <a target="_blank" href="https://letsdebug.net/">letsdebug.net</a>
        </p>
    </div>
</div>
