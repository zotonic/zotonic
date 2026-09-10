<div class="alert alert-info">
    <span class="glyphicon glyphicon-info-sign" aria-hidden="true"></span>
    {_ Below you can find the certificate path and instructions for trusting it. This information is shown only for websites in the development environment. _}
</div>

<div class="form-group">
    <label class="control-label" for="{{ #certificate_path }}">{_ Certificate file _}</label>
    <div class="input-group">
        <input id="{{ #certificate_path }}"
               class="form-control"
               type="text"
               value="{{ cert.certfile|escape }}"
               readonly>
        <span class="input-group-btn">
            <button class="btn btn-default"
                    type="button"
                    data-onclick-topic="model/clipboard/post/copy"
                    data-text="{{ cert.certfile|escape }}"
                    title="{_ Copy the certificate file path to the clipboard. _}">
                <span class="glyphicon glyphicon-copy" aria-hidden="true"></span>
                {_ Copy _}
            </button>
        </span>
    </div>
</div>

<details class="padding">
    <summary><strong>{_ How to trust this certificate on a development device _}</strong></summary>

    <p class="text-warning">
        <span class="glyphicon glyphicon-alert" aria-hidden="true"></span>
        {_ Trust this certificate only on devices used for development. Remove it when it is no longer needed. _}
    </p>

    <h4>{_ macOS _}</h4>
    <ol>
        <li>{_ Open the certificate file in Keychain Access and add it to the System keychain. _}</li>
        <li>{_ Open the imported certificate, expand Trust, and select Always Trust. _}</li>
        <li>{_ Authenticate when prompted, then restart the browser. _}</li>
    </ol>

    <h4>{_ Windows _}</h4>
    <ol>
        <li>{_ Double-click the certificate file and select Install Certificate. _}</li>
        <li>{_ Select Local Machine, then place the certificate in Trusted Root Certification Authorities. _}</li>
        <li>{_ Finish the wizard and restart the browser. Administrator permission may be required. _}</li>
    </ol>

    <h4>{_ Debian and Ubuntu _}</h4>
    <ol>
        <li>{_ Copy the certificate file to _} <code>/usr/local/share/ca-certificates/zotonic-self-signed.crt</code>.</li>
        <li>{_ Run _} <code>sudo update-ca-certificates</code> {_ and restart the browser. _}</li>
    </ol>

    <h4>{_ Fedora and Red Hat _}</h4>
    <ol>
        <li>{_ Copy the certificate file to _} <code>/etc/pki/ca-trust/source/anchors/zotonic-self-signed.crt</code>.</li>
        <li>{_ Run _} <code>sudo update-ca-trust</code> {_ and restart the browser. _}</li>
    </ol>

    <h4>{_ Firefox _}</h4>
    <ol>
        <li>{_ Open Settings, Privacy &amp; Security, Certificates, then View Certificates. _}</li>
        <li>{_ On the Authorities tab, import the certificate file and trust it to identify websites. _}</li>
        <li>{_ Restart Firefox. This separate import can be needed when Firefox does not use the operating system trust store. _}</li>
    </ol>

    <h4>{_ iOS and iPadOS _}</h4>
    <ol>
        <li>{_ Transfer the certificate file to the device, open it, and install the downloaded profile in Settings. _}</li>
        <li>{_ In Settings, open General, About, Certificate Trust Settings, and enable full trust for the certificate. _}</li>
    </ol>

    <h4>{_ Android _}</h4>
    <ol>
        <li>{_ Transfer the certificate file to the device. _}</li>
        <li>{_ In Settings, open the security or credentials section and install it as a CA certificate. Menu names vary by device. _}</li>
        <li>{_ Restart the browser. Some apps do not accept user-installed certificates. _}</li>
    </ol>
</details>

<br>
