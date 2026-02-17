# golangc - Go Compiler in C++

A Go 1.21+ compiler implemented in modern C++23, targeting x86-64 Windows executables (PE format).

## Building

### Prerequisites

- Visual Studio 2022 (MSVC v143+)
- CMake 3.25+
- vcpkg

### Build Steps

```powershell
cmake -B build -S . -DCMAKE_TOOLCHAIN_FILE="[vcpkg root]/scripts/buildsystems/vcpkg.cmake"
cmake --build build
```

### Running Tests

```powershell
cd build
ctest --output-on-failure
```

## Usage

```powershell
golangc [options] <source.go>

Options:
  --help          Show help message
  --version       Show version information
  --dump-tokens   Dump lexer tokens and exit
```

## Architecture

The compiler follows a multi-phase pipeline:

```
Source → Lexer → Parser → AST → Semantic Analysis → IR → Optimizer → Code Generator → PE Linker
```

See [docs/architecture.md](docs/architecture.md) for detailed design documentation.

## Project Status

See [docs/progress.md](docs/progress.md) for current development status.
