The current design is to have prod and qa be very different

Another idea would be to have the qa code path be exactly the same as the prod but it handles a hash conflict differently

So we could make a core migration flow that has a extension point for how to handle a hash conflict.

The prod flow perhaps just warns

The qa flow does something much more complex