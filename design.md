# Goal
Python-like language with static typing, maybe some type inference.
Goal is to allow clean OOP (interfaces, encapsulation) and also functional style.
Actually, maybe it should be a python-like language with OO, haskell-like type classes, and java-like interfaces.

# desired features
* classes
    * fields
    * methods
    * mutation
    * static fields and methods
* interfaces
    * multiple interface implementation
* no inheritance
* no null
* functions, local functions, lambdas
* parametric polymorphism
    * on classes
    * on functions
    * bounded types (would that even make sense with no inheritance?)
* custom infix operators with fixity declarations
* want something between java interfaces and haskell type classes for stuff like == and toString
* Primitive types behave like Composite types, no int vs Integer like java
* methods close over `this` so you can use `x.toString` as a `X -> String` function value
    * methods are curried functions with an implicit argument, and
     the example is a partial application.

## nice-to-haves
* tuples, tuple unpacking
    * would make the type system more intuitive since functions kind of take in a tuple
    * could make applications `expr expr` and allow curried definitions
        * `x.f a b` where `f` is a method of the `X` class and has type `A -> B -> C`
        * requires `x.toString` feature
* visibility and mutability modifiers
* haskell-like `deriving` mechanism for implementing interfaces automatically if a field has a type
that implements the interface. Auto-generates delegation based on the field, errors if 0 or more than 1
field implements the interface.
* Depending on how you do type classes, might want higher-kinded types for monads. Probably unnecessary.

# problems
* mutual recursion
    ```
    rec {
        f :: (Int) -> Int = function(x) { return g(x); };
        g :: (Int) -> Int = function(x) { return f(x); }
    }
    ```
  * functions only
* syntax for applying polymorphic functions
    * `equal<Int>(1,2)`
* how to represent `x.toString` partial applications in the type system.
    * `toString :: X -> () -> String`
    * Or maybe the expression will just have the method signature, and it'll be like methods
    are fields which have function types, and the function closes over `this`
* interface / type class (has own section)

# interface / type class design
* would like to be able to say something like `instance Eq IShape where a == b = a.area() == b.area()`
    * Then any IShapes could be compared for equality, regardless of the implementing class.
* Would need a resolution mechanism for something like `hexagon == circle` where it picks the common ancestor
implementation
    * based on dynamic or static type?
* Could just make something like the Eq type class where == takes 
two of the same type, but then subtyping would be an issue.
    * or would it? if you have no inheritance, there's only 1 concrete class type
* would need to distinguish between interfaces, which are a type, and type classes, which are
more like a constraint.
    * You can say `IShape x = ...`, but not `Eq x = ...`
* let's say you're making a Map class, and the key needs to be Ord. You can't just take in values of type Ord,
because you have no guarantee that they're the same type. Need a type constraint.
    * You'll have to make syntax for talking about type constraints in type annotations :(.
    * `def f<a implements Eq>(x :: a, y :: a) -> Bool { return equals<a>(x, y) }`
* Need haskell-like dictionary-passing type constraint system
    * mention type constraints in types (type contexts)
    * implicitly pass them around in the runtime
* possible syntax: `private const f<a> :: Eq<a> => (a, a) -> Bool = function(x, y) { return equals<a>(x,y) };`
    * same syntax for methods, functions, and variables
    * I feel like methods should have special syntax. Otherwise, how will you clearly distinguish between `this` being allowed
    in a method body, but not being allowed in a default definition for a non-function field?
        * could just make a special case for when RHS is a `function` expression, but that's messy
        * `method` keyword? Pretty much syntactic sugar for `const function`.
            * nice way to distinguish between function fields and actual methods. Maybe only method definitions can access
            `this` when defining at class-level.
* type class definition syntax
```
typeclass Eq<a> {
    equals<a> :: (a, a) -> Bool;
}
```
* type class instantiation syntax
```
instance Eq<IShape> {
    equals :: (IShape, IShape) -> Bool = function (x,y) {
        return equals(x.area(), y.area());
    }
}
```
* Then "in the wild", `equals` would look like it has type `equals<a> :: Eq<a> => (a,a) -> Bool`
* Would also like to be able to specify constraints at a class level. `class Ord<a> => Map<a>`
* At this point, do you even need java-like interfaces? Could just take in a generic `a` with the constraint `IShape<a>`
    * if you don't do java-like interfaces, you could use the `class Circle implements IShape` for `class Person implements Eq<Person>`
    * kind of leaving OOP territory though. It's becoming mutable records with functions that close over `this` and have
    special call syntax. Just keep java interfaces then. use haskell-like syntax within a class-body for instantiation
    of type classes. Or just do `class Hexagon implements IShape instantiates Eq<Hexagon>`
* can't have polymorphic methods in a type class with this syntax. How would you instantiate
`typeclass Foo<a> { foo<t,a> :: (a,t) -> Bool }`?
    * delete primary type variable(s)? `class Bar instantiates Foo<Bar> { ... foo<t> :: (Bar, t) = ... }` messy
    * make instantiation say `class Bar instantiates Foo<Bar> { ... foo<t, Bar> :: (Bar, t) = ... }` cleaner,
    more annoying to write
## summary / final decisions
* both java interfaces and haskell type classes
* haskell-like definition of type classes
```
typeclass Eq<a> {
    equals<a> :: (a, a) -> Bool;
}
```
* java-like instantiation of type classes
```
class Person instantiates Eq<Person> {
    private const age :: Int
    ...
    equals<Person> :: (Person, Person) -> Bool =
    function(a, b) { return equals<Int>(a.age, b.age); }
}
```

# thoughts
* maybe modifiers should be part of the type
    `makeFooSource<a> :: (Ord a, a extends Foo) => private const (List<a>) -> Source<Foo>`
* for java interface style, since there's no Object class, have a Show and Eq java-interface at first. print takes in a Show
# plan
* first, just do static python with equals and toString being done with interfaces and some instanceof's and casts
* then, add type classes.
* start out with no functions, just variable assignment and "magic" auto printing since there's no cyclic data
* add functions
* add classes
* add interfaces
* add type classes
* have a whole language the whole time. That means maintaining the parser, checker, and interpreter every step.