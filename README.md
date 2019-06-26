# Minsk C++ Fork

A C++ fork of the [Minsk compiler](https://github.com/terrajobst/minsk). 


#### Differences from the original Minsk

Although this fork stays as close to the original as possible, there are some deviations emerging from implementation, primarily due to the two languages being different (Oh the beauty of having a GC) and the human being behind being dumb.

- A dirty hack of a unified value system.

    `std::variant` comes in handy here to mimic `object` in C#, and `std::monostate` acts as a placeholder for `null`. The caveat is consumers/users of this hack have to know the underlying type stored in `std::variant` if they want to read the value. Then again C++ is a statically typed language.

- Redundancy in `GetChildren` ~~and `GetProperties`~~ virtual member functions. 

    The lack of reflection makes writing such functions in `SyntaxNode` ~~and `BoundNode`~~ base class~~es~~ impossible. ~~C++ doesn't even have the concept of property.~~ This problem is partly alleviated by conjuring up some template functions.

- The lowering process, i.e. `BoundTreeReriter` & `Lowerer` ~~, analyzes the existing bound tree and generates a new one.~~

    ~~This is mainly because `BoundNode`s exposing `std::unique_ptr`s directly would break encapsulation. `std::shared_ptr`s would cause another slew of problems. The original lowering process tries its best to reduce memory allocation while this one does not. As a result, `Lowerer::Flatten` has to be a non-static member function.~~ 
    
    I caved in. The ownership of `BoundNode`s between `BoundProgram` and `BoundGlobalScope` and `BoundScope` is sometimes hard to reason. Esp. between the former two, shared ownership cannot be ruled out. On a flip side this helps reducing memory allocation when rewriting the bound tree.

- Templates vs Generics

    While powerful, C++ template functions and classes are hard to write, and (maybe) even harder to reason about. And using `std::enable_if` and such to mimic type constraints is not very elegant in any way. (What? CONCEPTS are coming? What about that?)

- Postfix operations, i.e. `i++` & `i--`

    They are intentionally left out in the original project. Here they are added in as a fun side tweak. It stays close to C++ implementation of prefix operations, meaning `(i = 5)--` is valid syntax, but `(i--) = 5` is not. Prefix operations are not supported yet. 

- Static members

    In C++, the initilazation order of static variables across multiple files is undefined, which ends up causing weird initializing issues. As a workaround static member variables are wrapped in static member functions. 

- Windows only.

    Without .NET Core wrapper around system APIs, this fork relies on native win32 APIs to manipulate console color and cursor display. 

- Language differences.

    - `std::vector` stands in place for `IEnumerable` mostly.
    - An array of helper functions for `enum class`es etc.
    - Static class seems ungrounded in C++.

- CMake + Visual Studio + Ninja + MSBuild + ...

    Ok this is more of a rant. C++ (meta)build systems are nefariously horrible compared to .NET Core CLI ( and of course [Cargo](https://github.com/rust-lang/cargo/) ). After trying CMake on and off for quite some time this is the first step to hop on the wagon ( so that clang is back now hooray! ). As of now both Visual Studio build files (MSBuild) and CMake files co-exist. The former remain necessary for tests to work until some other test framework gets in the fold. 