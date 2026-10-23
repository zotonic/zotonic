[![Test](https://github.com/zotonic/zotonic/workflows/Test/badge.svg)](https://github.com/zotonic/zotonic/actions?query=workflow%3ATest)
[![Dialyzer](https://github.com/zotonic/zotonic/workflows/Dialyzer/badge.svg)](https://github.com/zotonic/zotonic/actions?query=workflow%3ADialyzer)
[![GitHub release](https://img.shields.io/github/release/zotonic/zotonic.svg?maxAge=3600?style=flat-square)](https://github.com/zotonic/zotonic/releases)
[![Crowdin](https://badges.crowdin.net/zotonic/localized.svg)](https://crowdin.com/project/zotonic)
<!--
[![Docs](https://github.com/zotonic/zotonic/workflows/Docs/badge.svg)](https://github.com/zotonic/zotonic/actions?query=workflow%3ADocs)
[![Join the chat at https://gitter.im/zotonic/zotonic](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/zotonic/zotonic?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
-->

Zotonic
=======

**Zotonic is the open source, high speed, real-time web framework  and content management system, built with Erlang.**

It is flexible, extensible and designed from the ground up to support dynamic, interactive websites and mobile solutions.

Zotonic is incredibly fast and wonderfully stable – suited for anything from basic websites to complex distributed applications. It offers an elegant backend for managing content with the flexibility that developers need to build truly amazing applications.

Installation
------------

* Download Zotonic from the [**official website**](https://zotonic.com/download)
  or start Zotonic from a [**Docker or Podman image**](https://zotonic.com/docs/1411/docker).
* Read the [**Installation chapter**](https://zotonic.com/docs/1526/getting-started#installation)
  in the documentation.

The development launchers `./start-docker.sh` and `./start-podman.sh` use your local
UID and GID for the container's Zotonic user. Start them as your normal user.
Override either ID when necessary, for example:

```sh
ZOTONIC_UID=1001 ZOTONIC_GID=1001 ./start-docker.sh
```

The same overrides work with `./start-podman.sh`. It loads
`docker-compose.podman.yml` to map the calling user to those container IDs using
Podman's `keep-id` namespace. The entrypoint initializes as container root and
then runs Zotonic as the selected user. The Podman bind mount uses `:Z` for
SELinux labeling. Use the launcher so both Compose files and the IDs are supplied.

Direct Docker Compose runs can set `ZOTONIC_UID` and `ZOTONIC_GID` explicitly;
without them, the entrypoint infers ownership from `rebar.config`. IDs must be
non-zero decimal numbers. Existing image groups are reused, while a UID belonging
to another image account is rejected. Overrides do not change host checkout
ownership; Docker user-namespace remapping can require additional host-side setup.

Documentation
-------------

You can find out more about Zotonic on http://zotonic.com, including:

* [**Official documentation**](https://zotonic.com/developer-guide).
* [**Release notes**](https://zotonic.com/docs/1643/release-notes).

Contributing
------------

Zotonic is an open source project, made possible by the community. If you’d like to contribute,
please read the [Contributing chapter](https://zotonic.com/docs/1642/contributing-to-zotonic)
in the documentation.

If you encounter any issues, please report them in our
[issue tracker](https://github.com/zotonic/zotonic/issues). If you discover
any security-related issues, please contact us at [security@zotonic.com](mailto:security@zotonic.com).

License
-------

Zotonic is released under the Apache 2.0 License. Please refer to the included
[LICENSE](LICENSE) file for more information.

Thanks
------

Thanks to the following services for supporting Open Source:

<a href="https://browserstack.com/"><img rel="Thanks to BrowserStack" src="https://raw.githubusercontent.com/zotonic/zotonic/master/doc/img/browserstack-logo.png" height="64" /></a> <a href="https://crowdin.com/"><img src="https://user-images.githubusercontent.com/38268/188396792-7142a245-7805-4654-aead-9ae337f6d977.svg" height="64" /></a>
