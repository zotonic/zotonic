[![Build Status](https://travis-ci.org/zotonic/zotonic.svg?branch=master)](https://travis-ci.org/zotonic/zotonic)
[![GitHub release](https://img.shields.io/github/release/zotonic/zotonic.svg?maxAge=3600?style=flat-square)](../../releases)
[![Join the chat at https://gitter.im/zotonic/zotonic](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/zotonic/zotonic?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Zotonic
=======

**Zotonic is the open source, high speed, real-time web framework  and content management system, built with Erlang.**

It is flexible, extensible and designed from the ground up to support dynamic, interactive websites and mobile solutions.

Zotonic is incredibly fast and wonderfully stable – suited for anything from basic websites to complex distributed applications. It offers an elegant backend for managing content with the flexibility that developers need to build truly amazing applications.

Start getting to know Zotonic on the [feature page](http://zotonic.com/features), have a look at our [introduction video](http://zotonic.com/page/750/video-introduction-to-zotonic) and check out the [gallery page](http://www.zotonic.com/gallery/735/screen-shot-gallery).

Installation
------------

* Download Zotonic from the [**official website**](http://zotonic.com/download). 
* Read the [**Installation chapter**](http://zotonic.com/docs/latest/developer-guide/installation.html) 
  in the documentation.

Documentation
-------------

You can find out more about Zotonic on http://zotonic.com, including:

* [**Official documentation**](http://zotonic.com/docs).
* [**Release notes**](http://zotonic.com/docs/latest/developer-guide/releasenotes/index.html).

Contributing
------------

Zotonic is an open source project, made possible by the community. If you’d like to contribute, 
please read the [Contributing chapter](http://zotonic.com/docs/latest/developer-guide/contributing.html)
in the documentation.

Zotonic in Docker
-----------------

Try out Zotonic in a [Docker](https://www.docker.com/) container. Zotonic images
are available on [Docker Hub](https://hub.docker.com/r/zotonic/zotonic/).

To get started quickly, start a `zotonic/zotonic` image on your Docker machine:

```bash
# use a tagged version, or branch
docker run -d -p 8000:8000 zotonic/zotonic:release-x.y.z

# or run latest version from master
docker run -d -p 8000:8000 zotonic/zotonic:latest
```

**Note** This image is for evaluation purposes, and not supposed
to be used in production.

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=zotonic&url=https://github.com/zotonic/zotonic&title=zotonic&language=en_GB&tags=github&category=software) 
