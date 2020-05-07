This is a C++ fork of the [Minsk compiler](https://github.com/terrajobst/minsk). 


### To build and run

#### Prerequisites

- Compiler with C++17 support
- CMake
- Ninja (Sorry MSBuid)
- vcpkg with the following installed:
    - [Catch2](https://github.com/catchorg/Catch2)
    - [cxxopts](https://github.com/jarro2783/cxxopts)
    - LLVM


#### Build

```cmd
mkdir build && cd build
cmake -G Ninja -DCMAKE_TOOLCHAIN_FILE=<vcpkg_root>/scripts/buildsystems/vcpkg.cmake -DVCPKG_TARGET_TRIPLET=x64-windows .. [1]
cmake --build . [2]
```

[1]: `-DVCPKG_TARGET_TRIPLET=x64-windows` is not needed if building for x86.

[2]: vcpkg might complain about dumpbin.exe missing but the build is completed.

#### Run

The executables can be found in `./build/Debug/`. Try it out with:

```cmd
cd Debug
mcfc.exe -p ..\..\samples\hello\hello.mcf
```

Then navigate to [./samples/hello/](./samples/hello/) directory and execute [build.cmd](./samples/hello/build.cmd).

### Design Difference

Although this fork stays as close to the original as possible, there are some choices specifically made to be different from Minsk. 

- Using directive

    It just feels not right to automatically compile every source file in the same directory without any checks. Here a `using` keyword is added to bring other source files into the fold. As of now it works with [./samples/hello/hello.mcf](./samples/hello/hello.mcf) ~~and [./samples/fib/test.mcf](./samples/fib/test.mcf)~~ but definitely needs more testing. In the hindsight it works more like Python's `import` than C#'s `using`. The downside is it pollutes the current source file with every symbol from imported sources. 

    A side effect is that it doesn't take multiple paths as input as all necessary source files should be properly imported. I consider this an improvement.

- LLVM backend

    Because why not. Still this is very much wading through the muddy waters of the huge LLVM world. Right now it only emits functions into an .obj file and not so shockingly needs to link against C runtime to turn into an executable. A sample is in the [./samples/hello/](./samples/hello/) directory. 

    Peeking through [./samples/lib.cpp](./samples/lib.cpp), it is obvious that a lot of heavy lifting, e.g. console input and string operations, is delegated to C++ code. It sounds very much like cheating. Then again in Minsk such work is done by utilizing .Net assemblies. 

    Another disadvantage here is that all runtime generated strings, either from calling `input()` or concatenating strings are currently stored in a C++ container. Without proper GC it might get bloated up rather quickly. 

- Postfix operations, i.e. `i++` & `i--`

    They are intentionally left out in the original project. Here they are added in as a fun side tweak. It stays close to C++ implementation of prefix operations, i.e. `(i = 5)--` is valid syntax, but `(i--) = 5` is not. Prefix operations are not supported yet. 

- Windows only.

    Without .NET Core wrapper around system APIs, this fork relies on native win32 APIs to manipulate console color and cursor display. 


### Implementation Detail

This section documents some interesting decisions made when forking Minsk, mostly due to the two languages being different (Oh the beauty of GC) and the human being behind the curtain being dumb.

- `std::variant` for a unified value system.

     It comes in really handy here to mimic `System.Object` in C#, and `std::monostate` acts as a placeholder for `null`. The caveat is consumers/users of this hack have to know the underlying type stored in `std::variant` if they want to read the value. Then again C++ is a statically typed language.

     This approach works well until it turns away from interpreter and hits the challenges of compiler. Minsk supports an `any` type which closely alignes with `object`, but `std::variant` doesn't translate well into LLVM representation. For now `any == any` and such convert `any` values into strings and do the corresponding operations using string semantics. 

- Spamming `std::string_view`

    Rather than one `System.String` class, C++ pleasantly presents `std::string`, `const char*`, `const string&` and a gazillion other ways (not really) to represent and manipulate a string and their semantics often overlap (to copy? to view? to modify? to consume?). `std::string_view` comes in between that acts as only a view with the potential to copy, i.e. the perfect glue type. The downside is it is non-owning and likely to raise dangling issues when not careful. 

- Code Redundancy

    Emerging from lack of reflection in C++, e.g. in class derived from `SyntaxNode`, or property, e.g. an array of getters in nearly every class. This problem is partly alleviated by conjuring up some templates. And some `std::function` magic. 

- CMake + Visual Studio + Ninja + MSBuild + ...

    Ok this is more of a rant. C++ (meta)build systems are infamously horrible compared to .NET Core CLI (and of course [Cargo](https://github.com/rust-lang/cargo/)). CMake is almost ubiquitously standard, and Ninja is nice and sleek, but things coule be better.