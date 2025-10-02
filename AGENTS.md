# AI Agent Instructions for protobuf

## Repository Overview

This repository contains an implementation of Google's Protocol Buffers in Haskell. It provides a pure Haskell implementation that uses GHC.Generics and does not require `.proto` files to function.

### Key Features
- Pure Haskell implementation of Protocol Buffers
- Uses GHC.Generics for automatic encoding/decoding
- No preprocessor or additional build steps required
- Supports Required, Optional, Repeated, and Packed fields
- Type-safe field tags using DataKinds

## Build System

This project uses the Cabal build system for Haskell.

### Building
```bash
cabal update
cabal build --enable-tests --enable-benchmarks
```

### Testing
```bash
cabal test --enable-tests
```

### Running Benchmarks
```bash
cabal bench
```

### Package Validation
```bash
cabal check
```

## Code Style Guidelines

### Language Extensions
This project commonly uses the following GHC language extensions:
- `DeriveGeneric` - For automatic Generic instance derivation
- `DataKinds` - For type-level numeric field tags
- `OverloadedStrings` - For Text literals
- `FlexibleContexts` and `FlexibleInstances` - For generic programming
- `TypeOperators` - For generic representation types

### Naming Conventions
- Use camelCase for function and variable names
- Use PascalCase for type names and constructors
- Field selectors in protobuf messages should be descriptive
- Type parameters typically use single letters (a, b, c) or meaningful names

### Code Structure
- Main library code is in `src/Data/ProtocolBuffers/`
- Test suite is in `tests/Main.hs`
- Benchmarks are in `bench/`
- Plugin code (currently commented out) is in `plugin/`

### Documentation
- Use Haddock-style comments for public APIs
- Include examples in documentation where appropriate
- Document complex type-level programming constructs

## Important Project Details

### Core Modules
- `Data.ProtocolBuffers` - Main public API
- `Data.ProtocolBuffers.Internal` - Internal implementation details
- `Data.ProtocolBuffers.Encode` - Encoding logic
- `Data.ProtocolBuffers.Decode` - Decoding logic
- `Data.ProtocolBuffers.Wire` - Wire format handling
- `Data.ProtocolBuffers.Types` - Core type definitions

### Field Types
- `Required n a` - Required field with tag n
- `Optional n a` - Optional field with tag n (uses Last monoid)
- `Repeated n a` - Repeated field with tag n (list of values)
- `Packed n a` - Packed repeated field with tag n
- `Value a` - Wraps primitive types
- `Message a` - Wraps nested message types

### Key Type Classes
- `Encode` - For encoding messages to binary format
- `Decode` - For decoding messages from binary format
- `HasField` - For getting and setting field values
- `EncodeWire` / `DecodeWire` - For primitive wire type encoding/decoding

### Testing Approach
- Uses Tasty test framework (tasty, tasty-hunit, tasty-quickcheck)
- QuickCheck properties for roundtrip testing
- HUnit tests for specific encoding/decoding cases
- Google reference test cases for compatibility

### Compatibility
The project targets multiple GHC versions as specified in CI:
- GHC 8.10.7
- GHC 9.0.2
- GHC 9.2.8
- GHC 9.4.8
- GHC 9.6.6
- GHC 9.8.4

Ensure changes are compatible with the oldest supported GHC version (8.10.7).

## Making Changes

### Before Making Changes
1. Understand the Generic programming approach used (GHC.Generics)
2. Review existing encoding/decoding implementations
3. Check for similar patterns in the codebase
4. Ensure changes maintain wire format compatibility

### After Making Changes
1. Run the full test suite: `cabal test`
2. Run cabal check: `cabal check`
3. Build with benchmarks enabled: `cabal build --enable-benchmarks`
4. Consider adding QuickCheck properties for new functionality
5. Update documentation if public APIs are modified

### Common Pitfalls
- Protocol Buffers has specific wire format requirements - test thoroughly
- Field tags must be unique within a message
- Type-level programming can be tricky - use `:kind` in GHCi to debug
- Changes to encoding must maintain backward compatibility
- Be careful with orphan instances

## Dependencies

Key dependencies include:
- `base` - Base Haskell library
- `bytestring` - Efficient byte strings
- `binary` - Binary serialization
- `text` - Text handling
- `unordered-containers` - Hash maps and sets
- `mtl` - Monad transformer library
- `deepseq` - For benchmarking

Avoid adding new dependencies unless absolutely necessary.

## Known Limitations

The following Protocol Buffers features are not currently implemented:
- Default values for Optional fields
- Extension fields
- Storing unknown fields (fields without a mapped tag)

When working on these areas, document any intentional limitations or design decisions.
