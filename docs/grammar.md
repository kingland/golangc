# Go Grammar Reference

Subset of the Go language specification grammar used by this compiler.
Based on the [Go Language Specification](https://go.dev/ref/spec).

## Source File Structure

```
SourceFile    = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
PackageClause = "package" PackageName .
PackageName   = identifier .
```

## Import Declarations

```
ImportDecl    = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
ImportSpec    = [ "." | PackageName ] ImportPath .
ImportPath    = string_lit .
```

## Top-Level Declarations

```
TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
Declaration   = ConstDecl | TypeDecl | VarDecl .
```

## Constant Declarations

```
ConstDecl     = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
ConstSpec     = IdentifierList [ [ Type ] "=" ExpressionList ] .
```

## Type Declarations

```
TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
TypeSpec     = AliasDecl | TypeDef .
AliasDecl    = identifier "=" Type .
TypeDef      = identifier Type .
```

## Variable Declarations

```
VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
```

## Function Declarations

```
FunctionDecl = "func" FunctionName Signature [ FunctionBody ] .
FunctionName = identifier .
FunctionBody = Block .
MethodDecl   = "func" Receiver MethodName Signature [ FunctionBody ] .
Receiver     = Parameters .
```

## Types

```
Type         = TypeName | TypeLit | "(" Type ")" .
TypeName     = identifier | QualifiedIdent .
TypeLit      = ArrayType | StructType | PointerType | FunctionType |
               InterfaceType | SliceType | MapType | ChannelType .
ArrayType    = "[" ArrayLength "]" ElementType .
SliceType    = "[" "]" ElementType .
MapType      = "map" "[" KeyType "]" ElementType .
ChannelType  = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
PointerType  = "*" BaseType .
```

## Statements

```
Statement    = Declaration | SimpleStmt | GoStmt | ReturnStmt |
               BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt |
               Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
               DeferStmt .
SimpleStmt   = EmptyStmt | ExpressionStmt | SendStmt |
               IncDecStmt | Assignment | ShortVarDecl .
```

## Expressions

```
Expression     = UnaryExpr | Expression binary_op Expression .
UnaryExpr      = PrimaryExpr | unary_op UnaryExpr .
PrimaryExpr    = Operand | PrimaryExpr Selector |
                 PrimaryExpr Index | PrimaryExpr Slice |
                 PrimaryExpr TypeAssertion | PrimaryExpr Arguments .
```

## Operator Precedence (highest to lowest)

```
5    *  /  %  <<  >>  &  &^
4    +  -  |  ^
3    ==  !=  <  <=  >  >=
2    &&
1    ||
```
