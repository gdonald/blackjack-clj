## Blackjack for Clojure

This project provides the [Blackjack](https://en.wikipedia.org/wiki/Blackjack) card game written in [Clojure](https://clojure.org/).

### Screenshots

![Screenshot 1](https://raw.githubusercontent.com/gdonald/blackjack-clj/refs/heads/main/ss1.png)

![Screenshot 2](https://raw.githubusercontent.com/gdonald/blackjack-clj/refs/heads/main/ss2.png)

### Install

You will need the [Clojure CLI](https://clojure.org/guides/install_clojure) installed.

On macOS you can install it via [Homebrew](https://brew.sh/):

    brew install clojure/tools/clojure

Clone the repository:

    git clone https://github.com/gdonald/blackjack-clj.git
    cd blackjack-clj

#### Running blackjack:

Once cloned you can run the game like this:

    ./blackjack.sh

Or directly via the Clojure CLI:

    clojure -M:run

#### Running tests:

Tests are written using [clojure.test](https://clojure.github.io/clojure/clojure.test-api.html) and executed with [cognitect-labs/test-runner](https://github.com/cognitect-labs/test-runner).  Coverage is generated using [Cloverage](https://github.com/cloverage/cloverage).

To run the tests and build a coverage report:

    ./run_tests.sh

#### Going broke:

If you run out of money, delete `blackjack.txt` and restart blackjack.  You will get a free $100 for another try.

### Bugs / Issues / Feature Requests

Please report any bugs or issues you find:

[https://github.com/gdonald/blackjack-clj/issues](https://github.com/gdonald/blackjack-clj/issues)

### License

[![GitHub](https://img.shields.io/github/license/gdonald/blackjack-clj?color=aa0000)](https://github.com/gdonald/blackjack-clj/blob/main/LICENSE)

### Other Blackjack Implementations:

I've written Blackjack in [some other programming languages](https://github.com/gdonald?tab=repositories&q=blackjack&type=public&language=&sort=stargazers) too.  Check them out!
