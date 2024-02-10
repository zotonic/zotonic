<div class="form-group">
    {% button class="btn btn-outline-secondary" text=_"Rebuild sitemap"
              action={postback
                    postback=`sitemap_rebuild`
                    delegate=`mod_seo_sitemap`
                }
    %}
    <p class="help-block">{_ Rebuild the sitemap index for all pages. _}</p>
</div>
