<div class="widget" id="development-editor-settings">
    <div class="widget-header">{_ Source editor _}</div>
    <div class="widget-content">
        <p>{_ Open templates in your working editor. The local editor is saved on the Zotonic machine and shared by its development sites. Browser URLs and host paths are saved in this browser for this site. _}</p>
        <p data-editor-detection role="status">{_ Detecting editors… _}</p>
        <form id="development-editor-form"
              data-saved="{_ Editor settings saved. _}"
              data-open-requested="{_ Editor launch requested. _}"
              data-open-failed="{_ Could not open the editor. Check the editor and path mapping. _}"
              data-failed="{_ Could not load or save editor settings. _}"
              data-choose="{_ Choose an editor; no unambiguous default was found. _}"
              data-container="{_ This server uses browser launches only. Configure your host editor and path mapping below. _}"
              data-unavailable="{_ The preferred editor is unavailable or needs an existing editor server. _}">
            <fieldset disabled>
                <div class="form-group">
                    <label for="development-editor-mode">{_ Open files using _}</label>
                    <select id="development-editor-mode" name="mode" class="form-control">
                        <option value="auto">{_ Automatic detection on the Zotonic machine _}</option>
                        <option value="local">{_ Selected editor on the Zotonic machine _}</option>
                        <option value="browser">{_ Browser URL to an editor on this computer _}</option>
                    </select>
                </div>
                <div class="form-group" data-editor-local>
                    <label for="development-editor-local">{_ Local editor _}</label>
                    <select id="development-editor-local" name="local" class="form-control"></select>
                    <p class="help-block">{_ Local launches use the OS account and environment of the running Zotonic node. _}</p>
                </div>
                <div data-editor-browser hidden>
                    <div class="form-group">
                        <label for="development-editor-preset">{_ Code editor _}</label>
                        <select id="development-editor-preset" name="preset" class="form-control">
                            <option value="">{_ Choose an editor _}</option>
                            <option value="vscode">Visual Studio Code</option>
                            <option value="vscodium">VSCodium</option>
                            <option value="sublime">Sublime Text</option>
                            <option value="custom">{_ Custom URL / Vim helper _}</option>
                        </select>
                        <p class="help-block">{_ Sublime and Vim require a URL handler installed on this computer. _}</p>
                    </div>
                    <div class="form-group" data-editor-custom hidden>
                        <label for="development-editor-url">{_ Editor URL pattern _}</label>
                        <input id="development-editor-url" name="url" type="text" class="form-control"
                               placeholder="editor://open?file={file}&amp;line={line}&amp;column={column}">
                        <p class="help-block">{_ Use {file} for a URL-encoded filename, {path} for an encoded path preserving slashes, and {line} and {column} for the position. _}</p>
                    </div>
                    <div class="form-group">
                        <label for="development-editor-server-root">{_ Source root on the server or in the container _}</label>
                        <input id="development-editor-server-root" name="server_root" type="text" class="form-control" placeholder="/app/zotonic">
                    </div>
                    <div class="form-group">
                        <label for="development-editor-host-root">{_ Source root on this computer _}</label>
                        <input id="development-editor-host-root" name="host_root" type="text" class="form-control" placeholder="/Users/you/Sites/zotonic">
                        <p class="help-block">{_ Leave both roots empty when the paths are identical. Files outside the mapped root will not be opened. _}</p>
                    </div>
                </div>
                <button type="submit" class="btn btn-primary">{_ Save _}</button>
                <button type="button" class="btn btn-default" data-editor-test>{_ Test with this template _}</button>
                <p class="help-block" data-editor-status role="status"></p>
            </fieldset>
        </form>
    </div>
</div>
{% javascript %}
    zotonicDevelopmentEditor.settings(document.getElementById('development-editor-form'));
{% endjavascript %}
