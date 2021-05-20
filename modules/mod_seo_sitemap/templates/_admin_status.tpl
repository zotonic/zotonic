<div class="form-group">
    {% button class="btn btn-default" text=_"Rebuild sitemap"
              action={postback
                    postback=`sitemap_rebuild`
                    delegate=`mod_seo_sitemap`
                }
    %}
    <span class="help-block">{_ Rebuild the sitemap index for all pages. _}</span>
</div>
