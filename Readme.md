# Abstract
Writing C# functional code has become easier with each new release of the language (i.e. nullable ref types, tuples, switch expr, ...).
This document presents a review of the current status of *basic* functional features for C# 8.0.
It focuses mostly on syntax and aims to achieve its goal mostly by the usage of code examples.
It doesn't touches on more advanced arguments as Monad, Functors, etc ...

Haskell has been chosen as the 'comparison' language (and examples from [here](http://learnyouahaskell.com/).
This is not intended as a statement of value of one language vs the other.
The languages are profoundly  different in underlying philosophies.
They both do much more than what is presented here (i.e. C# supports OO and imperative style, while Haskell goes much deeper in type system power etc...).

I also present samples of usage of [language-ext](https://github.com/louthy/language-ext) to show what can be achieved by C# + a functional library.


## Using a library of functions
In C# you can use static functions as if they were 'floating' by importing the static class they are defined to with the syntax `using static CLASSNAME`. Below an example of both importing `Console.WriteLine` and using it.

```csharp
using System;
using LanguageExt;
using LanguageExt.TypeClasses;
using LanguageExt.ClassInstances;
using System.Collections.Generic;

using static System.Console;
using static System.Linq.Enumerable;
using static System.Math;
using static LanguageExt.Prelude;
using System.Runtime.CompilerServices;
using System.Security.Cryptography.X509Certificates;
using System.Drawing;

public static class Core {
    static void UseFunc() => WriteLine("System.WriteLine as a floating function");
```

## Writing simple functions
In Haskell, a simple function might be written as:
```haskell
   square :: Num a => a -> a

   square x = x * x
```

Which in C# would look like the below.

```csharp
    static int Square(int x) => x * x; 
```

As an aside, note that we lose the generality of the function (i.e. we need a different one for doubles).This is due to the lack of ad-hoc polymorphism in C#.
By using language-ext, you can fake it so that it looks like this:

```csharp
    static A Square<NumA, A>(A x) where NumA : struct, Num<A> => default(NumA).Product(x, x);

    static void SquarePoly() {
        WriteLine(Square<TInt, int>(2));
        WriteLine(Square<TDouble, double>(2.5));
    }
```

In Haskell, it is conventional to write the type of a function before the function itself. In C# you can use `Func` and `Action` to
achieve a similar goal (aka seeing the types separately from the function implementation), despite with more verbose syntax, as below:

```csharp
    static Func<int, int> SquareF = x => x * x;
```

## Pattern matching
Here is an example in Haskell:

```haskell
    lucky :: (Integral a) => a -> String  
    lucky 7 = "LUCKY NUMBER SEVEN!"  
    lucky x = show x
```

In C#, you could write it in a few ways. Either with a switch expression ...

```csharp
    static Func<int, string> Lucky = x => x switch {
            7 => "LUCKY NUMBER SEVEN!",
            _ => x.ToString()
        };
```

Or with a normal function:

```csharp
    static string Lucky1(int x) => x switch {
            7 => "LUCKY NUMBER SEVEN!",
            _ => x.ToString()
    };
```

Or using the ternary operator (a perennial favorite of mine).

```csharp
    static string Lucky2(int x) => x == 7 ? "LUCKY NUMBER SEVEN!"
                                          : x.ToString();
```

Pattern matching on tuples works with similar syntax:

```csharp
    static bool And(bool x, bool y) =>
        (x, y) switch
        {
            (true, true) => true,
            _            => false
        };

    
```

        
In Haskell you can match on lists as follows:

```haskell
    sum :: Num a => [a] -> a
    sum [] = 0
    sum (x:xs) = x + sum xs
```
Writing this in standard C# looks like this:

```csharp
    static int Sum(IEnumerable<int> l) => l.Count() == 0
                                            ? 0
                                            : l.Head() + Sum(l.Tail());
```

Language-ext allows you a simpler syntax, with more flexibility on what you can match against:

```csharp
    static int Sum1(IEnumerable<int> list) =>
        match(list,
               () => 0,
               (x, xs) => x + Sum1(xs));
```

Obviously you always have to be careful with recursion in C# ([here](https://github.com/dotnet/csharplang/issues/2544)).
Better use the various methods on `Enumerable`.

## Guards (and case expressions)
Let's explore guards. Case expressions have an identical translation in C#.
            
In Haskell guards are used as below:

```haskell
    bmiTell :: (RealFloat a) => a -> a -> String  
    bmiTell weight height  
            | weight / height ^ 2 <= 18.5 = "Under"  
            | weight / height ^ 2 <= 25.0 = "Normal"  
            | weight / height ^ 2 <= 30.0 = "Over"  
            | otherwise                   = "Way over"
```
Which can be coded in C# as:

```csharp
    static string BmiTell(double weight, double height) =>
        (weight, height) switch
        {
            _ when Pow(weight / height, 2) <= 18.5 => "Under",
            _ when Pow(weight / height, 2) <= 25.0 => "Normal",
            _ when Pow(weight / height, 2) <= 30.0 => "Over",
            _                                      => "Way over"
        };
```

Obviously this is quite bad. You would like something more like:

```haskell
    bmiTell :: (RealFloat a) => a -> a -> String  
    bmiTell weight height  
            | bmi <= skinny = "Under"  
            | bmi <= normal = "Normal"  
            | bmi <= fat    = "Over"  
            | otherwise     = "Way Over"  
            where bmi = weight / height ^ 2  
                  skinny = 18.5  
                  normal = 25.0  
                  fat = 30.0 
```
But it is not trivial in C# to declare variables in expression bodied members.
You can either move it to a normal method or abuse the LINQ query syntax. Both shown below.
Notice that this is more similar to `let` expressions in Haskell, as they come before the expression, not after.

```csharp
    static string BmiTell1(double weight, double height) {
        double bmi = Pow(weight / height, 2),
               skinny = 18.5,
               normal = 25.0,
               fat = 30;

        return bmi switch
        {
            _ when bmi <= skinny => "Under",
            _ when bmi <= normal => "Normal",
            _ when bmi <= fat    => "Over",
            _                    => "Way over"
        };
    }

    static string BmiTell2(double weight, double height) =>
        (   from _ in "x"
            let bmi     = Pow(weight / height, 2)
            let skinny  = 18.5
            let normal  = 25.0
            let fat     = 30
            select   bmi <= skinny ? "Under"
                   : bmi <= normal ? "Normal"
                   : bmi <= fat    ? "Over"
                   :                 "Way over").First();
```

## Product types (aka Records)
In Haskell you define a product type as below:

```haskell
    data Person = Person { firstName :: String  
                             , lastName :: String  
                             , age :: Int
                             } deriving (Show)
```

In C#, it is currently complicated to define an immutable product type with structural equality, structural ordering and efficient hashing.

In essence, you have to implement a bunch of interfaces and operators, somehow similar to below (and I am not implementing ordering, and it is probably not too efficient either).

```csharp
    public readonly struct PersonData: IEquatable<PersonData> {
        public readonly string FirstName;
        public readonly string LastName;
        public readonly int Age;

        public PersonData(string first, string last, int age) => (LastName, FirstName, Age) = (last, first, age);

        public override int GetHashCode() => (FirstName, LastName, Age).GetHashCode();
        public override bool Equals(object other) => other is PersonData l && Equals(l);
        public bool Equals(PersonData oth) => LastName == oth.LastName && FirstName == oth.FirstName && Age == oth.Age;
        public static bool operator ==(PersonData lhs, PersonData rhs) => lhs.Equals(rhs);
        public static bool operator !=(PersonData lhs, PersonData rhs) => !(lhs == rhs);
    }
```

If you use a `struct`, you are still open to someone simply newing it using the default constructor, that you can't make `private`.

Using a class (as below) avoids that, but loses the pass by value semantic.

```csharp
    public class PersonData1 : IEquatable<PersonData1> {
        public readonly string FirstName;
        public readonly string LastName;
        public readonly int Age;

        public PersonData1(string first, string last, int age) => (LastName, FirstName, Age) = (last, first, age);

        public override int GetHashCode() => (FirstName, LastName, Age).GetHashCode();
        public override bool Equals(object oth) => oth is PersonData l && Equals(l);
        public bool Equals(PersonData1 other) => LastName == other.LastName && FirstName == other.FirstName && Age == other.Age;
        public static bool operator ==(PersonData1 lhs, PersonData1 rhs) => lhs.Equals(rhs);
        public static bool operator !=(PersonData1 lhs, PersonData1 rhs) => !(lhs == rhs);
    }
```

So, there is no easy fix. Using Language-ext you can do it much more simply, by inheritance.
But obviously it uses IL generation that is slow the first time around. Try running the code and notice the delay when IL generating.

```csharp
    public class PersonData2 : Record<PersonData2> {
        public readonly string FirstName;
        public readonly string LastName;
        public readonly int Age;

        public PersonData2(string first, string last, int age) => (LastName, FirstName, Age) = (last, first, age);

    }
```

## Sum types (aka Discriminated Union)
In Haskell you write:

```haskell
    data Shape =
                Circle Float Float Float
            | Rectangle Float Float Float Float
            | NoShape
            deriving (Show)  
```

There is no obvious equivalent in C#, and different libraries has sprung up to propose possible solutions (but not language-ext)
(i.e. [here](https://github.com/Galad/CSharpDiscriminatedUnion) or [here](https://github.com/mcintyre321/OneOf)).

One possible 'pure language' implementation, not considering structural equality/ordering/hash, follows:

```csharp
    abstract class Shape {

        public sealed class NoShape : Shape { }
        public sealed class Circle : Shape {
            internal Circle(double r) => Radius = r;
            public readonly double Radius;
        }
        public sealed class Rectangle : Shape {
            internal Rectangle(double height, double width) => (Height, Width) = (height, width);
            public readonly double Height;
            public readonly double Width;
        }
    }

    static Shape.Circle Circle(double x)                 => new Shape.Circle(x);
    static Shape.Rectangle Rectangle(double x, double y) => new Shape.Rectangle(x,y);
    static Shape.NoShape NoShape()                       => new Shape.NoShape();
```

You can then pattern match on it in various obvious ways:

```csharp
    static double Area(Shape s) => s switch
    {
        Shape.NoShape _                  => 0,
        Shape.Circle {Radius: var r }    => Pow(r, 2) * PI,
        Shape.Rectangle r                => r.Height * r.Width,
        _                                => throw new Exception("No known shape")
    };

    static void CalcAreas() {
        var c = Circle(10);
        var r = Rectangle(10, 3);
        var n = NoShape();

        WriteLine(Area(c));
        WriteLine(Area(r));
        WriteLine(Area(n));
    }
```

## Maybe (Or Option) type
In Haskell you write:

```Haskell
     f::Int -> Maybe Int
     f 0 = Nothing
     f x = Just x

     g::Maybe Int -> Int
     g Nothing  = 0
     g (Just x) = x

```

In C#, this easily translates to `Nullable` value and reference types. Assume you have `Nullable = enabled` in your project

```csharp
    static int? F(int i) => i switch {
        0 => new Nullable<int>(),
        _ => i
    };

    static int G(int? i) => i ?? 0;

    
```

## Full program

Let's finish with a semi-working version of Hangman,
from an exercise in [Haskell programming from first principles](http://haskellbook.com/).
    

## Conclusion
Let's wrap all the samples with a `Main` function.
    

```csharp
    static void Main() {
        UseFunc();
        DoublePoly();
        WriteLine(Square(2) == SquareF(2));
        WriteLine(Sum (new[] { 1, 2, 3, 4 }) == Sum1(new[] { 1, 2, 3, 4 }));
        WriteLine(BmiTell(80, 100) == BmiTell1(80, 100));
        WriteLine(BmiTell(80, 100) == BmiTell2(80, 100));

        WriteLine(new PersonData("Bob", "Blake", 40)     == new PersonData("Bob", "Blake", 40));
        WriteLine(new PersonData1("Bob", "Blake", 40)    == new PersonData1("Bob", "Blake", 40));
        WriteLine("Before IL gen");
        WriteLine(new PersonData2("Bob", "Blake", 40)    == new PersonData2("Bob", "Blake", 40));
        WriteLine(new PersonData2("Alphie", "Blake", 40) <= new PersonData2("Bob", "Blake", 40));
        WriteLine("Already genned");
        WriteLine(new PersonData2("Bob", "Blake", 40) == new PersonData2("Bob", "Blake", 40));
        WriteLine(new PersonData2("Alphie", "Blake", 40) <= new PersonData2("Bob", "Blake", 40));

        CalcAreas();

    }

}
```
