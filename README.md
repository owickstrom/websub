<div align="center">
<h1>WebSub</h1>
</div>

<p align="center">
<em>A <a href="https://www.w3.org/TR/websub/">WebSub (RFC7230)</a> client for Haskell</em>
</p>


## Overview

**This library is a work in progress!** The plan is to
provide an easy-to-use interface for subscribing to WebSub topics in Haskell
web applications, using a WAI middleware for callbacks.

The most basic subscriber use case works now, and you can see an example in
[app/Main.hs](app/Main.hs). The project is [run here at
GitHub](https://github.com/owickstrom/websub/projects/1), see the board for
status and progress.


## Subscription

The following diagram shows how the subscription can change state.

![Subscription state changes](graphics/subscription.uml.png)

## Contributing

I've just started hacking on this, so if you're interested in helping out or
giving feedback, post an issue and we'll take it from there.

### Code Style

This project uses [hindent](https://github.com/commercialhaskell/hindent) to
do consistent formatting. You can run hindent on all source files like so:

``` bash
make indent
```

## License

[Mozilla Public License v2.0](LICENSE).
