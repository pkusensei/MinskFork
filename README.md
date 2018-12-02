# Minsk C++ Fork

A C++ fork of the [Minsk compiler](https://github.com/terrajobst/minsk). 


#### Differences from the original Minsk

Although this fork stays as close to the original as possible, there are some deviations emerging from implementation, primarily due to the two languages being different (Oh the beauty of having a GC) and the human being behind being dumb.

- A dirty hack of a unified value system.

    `std::variant` comes in handy here to mimic `object` in C#, and `std::monostate` acts as a placeholder for `null`. The caveat is consumers/users of this hack has to know the underlying type stored in `std::variant` if they want to read the value. Then again C++ is a statically typed language.

- Redundancy in `GetChildren` and `GetProperties` virtual member functions. 

    The lack of reflection makes writing such functions in `SyntaxNode` and `BoundNode` base classes impossible. C++ doesn't even have the concept of property. 

- The lowering process, i.e. `BoundTreeReriter` & `Lowerer`, analyzes the existing bound tree and generates a new one. 

    This is mainly because `BoundNode`s exposing `std::unique_ptr`s directly would break encapsulation. `std::shared_ptr`s would cause another slew of problems. The lowering process tries its best to reduce memory allocation. As a result, `Lowerer::Flatten` has to be a non-static member function.

- Language differences.

    - `std::vector` stands in place for `IEnumerable` mostly.
    - An array of helper functions for `enum class`es etc.
    - Static class seems ungrounded in C++.