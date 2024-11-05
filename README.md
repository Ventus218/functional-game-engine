# Functional Game Engine

This is an experimental project created in my free time to advance my functional programming skills!

The goal of this project is to build a super-simple game engine adopting a purely functional programming paradigm.

## Overview

The engine exploits the `cats` and `cats-effect` libraries just for their stack-safe functionality, but it aims to remain simple.

Although it currently lacks fundamental capabilities (such as rendering or input handling), the engine can run multiple game objects with user-defined behaviors.

## Example Usage (Bouncy Ball)

You can run a sample simulation using the following command:

```sh
sbt run
```

The example simulates a bouncing ball affected by gravity. Each frame, the ball's coordinates are printed to the console. The ball accelerates downward due to gravity until it hits the ground (coordinate `0`), at which point it bounces back up.
