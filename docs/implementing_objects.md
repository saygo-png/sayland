# Implementing objects

An `Object` represents a wayland interface that can be used as an object.

Here is an example object:

```hs
instance Object Wp_fifo_manager_v1 where
  onRequest manager msg@Request_wp_fifo_manager_v1_destroy = do
    forwardMessage manager msg
    dropObject manager.wlid
  onRequest manager msg@(Request_wp_fifo_manager_v1_get_fifo fifoId surfaceId) = do
    getInterface surfaceId >>= \case
      Just _ -> do
        registerObject Wp_fifo_v1{wlid = fifoId, fifoSurface = surfaceId}
        forwardMessage manager msg
      Nothing -> protocolErrorG Err_invalid_object $ "wp_fifo_manager_v1.get_fifo: surface `" <> fromString (show surfaceId) <> "` does not exist"

  onEvent _ = \case {}
```

Objects have two methods on them; `onRequest` and `onEvent`. The first runs on requests, the other on events.
Requests and events are collectively called messages.
These methods are also referred to as handlers.

Handlers are defined for both compositors and clients at the same time! This is done because a lot of the behaviour is actually shared between both.
To deal with the differences we use functions that understand the relation of who is sending what.

`forwardMessage` only sends a message when the handler is run by a sender of the event/request
In the above example on `Request_wp_fifo_manager_v1_destroy`: `forwardMessage` only sends a message on the client.
The function does nothing when ran on the server. This is because it's a request, and only clients send requests.

`protocolError` and `protocolErrorG` always throw, but on the server side these errors are caught and sent to the client instead.

There are also 2 escape hatch functions that allow you to write code on only one side
`onServer` for server code and `onClient` for client code.
If you wish, you could write all the logic explicitly for both sides using these functions.
To send messages while doing this you can use `sendMessage` which is a "dumb" version of `forwardMessage` that ignores sides.
You can still use `protocolError` and `protocolErrorG` in this style.

If you wish to not implement an interface write:

```hs
instance (Unsatisfiable (Text "Your reason for not implementing")) => Object Wp_fifo_manager_v1
```

This provides compile time errors for people who attempt to use this interface.
Generally this should only be used for deprecated interfaces which aren't supposed to be used anymore.

If you want to avoid implementing just some events or requests, use the `stub` function and `forwardMessage`.
Though this generally shouldn't be done as it makes the implementation partial.
Either implement fully or don't in most cases.

```hs
instance Object Xdg_popup where
  onRequest obj msg@Request_xdg_popup_destroy = do
    stub obj msg
    forwardMessage obj msg
```

The exception to this rule is interfaces which have deprecated messages. In that case use `stubDeprecated` instead

## General guidelines

### Avoid catchall matches

```hs
  onEvent obj msg = do
    -- ...
```

This is because as protocols change they will add or remove methods, and these catchalls will not reflect that in warnings.
Match every event and request explicitly. If an interface has no events or requests, match with an empty lambdaCase.
The exception here is `stub` as it already marks something as unfinished.

### Write Vim folds

We divide protocols into interfaces and implementations using Vim fold markers. These are the comments like `-- Implementations {{{` and `-- }}}`.
Look at existing protocols to see how to format and use them.

### Don't use Prelude

We use a custom Prelude. You can import it from `Sayland.Internal.Prelude` Prelude.
There are custom hlint rules to indicate proper use of it.
