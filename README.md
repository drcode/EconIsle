# qlkit-todo-demo

This is a "batteries included" demo of qlkit- [Try a live version here](http://forwardblockchain.com/qlkit-todomvc/)

Please read the [recommended qlkit introductory article](https://medium.com/p/79b7b118ddac) for a walkthrough of this application.

## Setup

To get an interactive development environment run:

    clojure -m figwheel.main -b dev -r

and open your browser at [localhost:9500](http://localhost:9500/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

---
_Copyright (c) Conrad Barski. All rights reserved._
_The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php), the same license used by Clojure._

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
