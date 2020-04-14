This is a C++ fork of the [Minsk compiler](https://github.com/terrajobst/minsk). 


### To build and run

#### Prerequisites

- Compiler with C++17 support
- CMake
- Ninja (Sorry MSBuid)
- vcpkg with [Catch2](https://github.com/catchorg/Catch2) installed


#### Build

```cmd
mkdir build && cd build
cmake -G Ninja -DCMAKE_TOOLCHAIN_FILE=<vcpkg_root>/scripts/buildsystems/vcpkg.cmake -DVCPKG_TARGET_TRIPLET=x64-windows .. [1]
cmake --build . [2]
```

[1]: `-DVCPKG_TARGET_TRIPLET=x64-windows` is not needed if building for x86.

[2]: vcpkg complains about dumpbin.exe missing but the build is completed.

#### Run

The executables can be found in `./build/Debug/`. Try it out with:

```cmd
cd Debug
mcfc ../../samples/hello/helloworld.mcf
```

Or fire up VS Code from source directory, navigate to one of the sample files, and `ctrl+shift+P` to run `mcfc` task. 


### Design Difference

Although this fork stays as close to the original as possible, there are some choices specifically made to be different from Minsk. 

- Using directive

    It just feels not right to automatically compile every source file in the same directory without any checks. Here a `using` keyword is added to bring other source files into the fold. As of now it works with [/samples/hello/helloworld.mcf](/samples/hello/helloworld.mcf) and [/samples/fib/test.mcf](/samples/fib/test.mcf) but definitely needs more testing. In the hindsight it works more like Python's `import` than C#'s `using`. The downside is it pollutes the current source file with every symbol from imported sources. 

- Postfix operations, i.e. `i++` & `i--`

    They are intentionally left out in the original project. Here they are added in as a fun side tweak. It stays close to C++ implementation of prefix operations, i.e. `(i = 5)--` is valid syntax, but `(i--) = 5` is not. Prefix operations are not supported yet. 

- Windows only.

    Without .NET Core wrapper around system APIs, this fork relies on native win32 APIs to manipulate console color and cursor display. 


### Implementation Detail

This section documents some interesting decisions made when forking Minsk, mostly due to the two languages being different (Oh the beauty of GC) and the human being behind the curtain being dumb.

- `std::variant` for a unified value system.

     It comes in really handy here to mimic `object` in C#, and `std::monostate` acts as a placeholder for `null`. The caveat is consumers/users of this hack have to know the underlying type stored in `std::variant` if they want to read the value. Then again C++ is a statically typed language.

- Spamming `std::string_view`

    Rather than one `System.String` class, C++ pleasantly presents `std::string`, `const char*`, `const string&` and a gazillion other ways (not really) to represent and manipulate a string and their semantics often overlap (to copy? to view? to modify? to consume?). `std::string_view` comes in between that acts as only a view with the potential to copy, i.e. the perfect glue type. The downside is it is non-owning and likely to raise dangling issues when not careful. 

- Code Redundancy

    Emerging from lack of reflection in C++, e.g. in class derived from `SyntaxNode`, or property, e.g. an array of getters in nearly every class. This problem is partly alleviated by conjuring up some templates. And some `std::function` magic. 

- Templates vs Generics

    Templates are powerful, templates are fun, templates are hard to write, and (maybe) even harder to reason about. Also using `std::enable_if` and type traits to mimic type constraints is not very elegant in any way. (where is CONCEPTS)

- CMake + Visual Studio + Ninja + MSBuild + ...

    Ok this is more of a rant. C++ (meta)build systems are infamously horrible compared to .NET Core CLI (and of course [Cargo](https://github.com/rust-lang/cargo/)). CMake is almost ubiquitously standard, and Ninja is nice and sleek, but things coule be better.