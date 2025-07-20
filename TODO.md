# Thoughts

1. haskell syntax?
2. customizable operator? (partial order! With generated state-machine for operator grammar)

# Traits And Links

## 成员函数的亲疏性

1. 成员函数是this为第一个参数的语法糖，让"."去查找全局名称，而不是类型A内部的名称。（彻底无亲疏之别）
1. 成员函数不可拓展（彻底隔离亲疏）
1. 类似Rust，成员函数可以拓展，但仅限内部trait。（折中）
 
rust 中，struct 可以隐式转换为实现的 trait ，且不能为 struct 实现2次同一个trait，灵活性偏低，且有奇奇怪怪的orphan rule。

sight 为了解决这个问题，为 impl 引入名称：

```
// Trait 实现
impl a MyStruct : MyTrait {
    fn foo() { ... }
}

// 成员函数 
impl b MyStruct {
    fn test() {

    }
}
```

通过 use impl a ; 将 a 设置为当前激活的 impl 。

```
use impl a
```

多个 `use`:

```
use impl a, b, c
```

编译期需要解析 a, b, c 中的 impl 是否存在二义性。如果有，则必须指定优先级。

```
use impl a then b then c
```

这样表示先看 a ，再看 b ，再看 c

同一个 block 级别只允许1个 `use impl`。

```
use impl a 
use impl b // error
```

创建新的 block 可以用 super 引用上一个 block 的 use。

```
use impl a
{
    use impl super then b
}
```

impl 可以直接内联进 use 里面：


```
use impl MyStruct : MyTrait {
    ...
} then a, b then impl MyStruct {
    ...
}
```

# Incremental Trait Impl

**动机**： rust 无法为一个外部类型实现一个外部trait，也就是所谓孤儿规则。孤儿规则的为了避免一个类型+trait 有多个实现。但这个做法显然有点矫枉过正了。

rust 绕开孤儿规则的办法是 newtype 模式。麻烦丑陋。

**提案**：允许为外部类型实现外部trait。实现可以选择性标注为pub，则该实现在子模块也可见。

```
mod a {
    
    pub impl Display for String {
        ...
    }

    mod b {
        ...
        let str = "foo";
        str.display(); 
        ...
    }

}

```

impl 可以具名，用来打包导出

# Formmatter fills defaults

默认是可以修改的，格式化器会帮你填充默认值。

例如；
```
@default pub
struct A {
    foo
}
```

格式化后

```
@default pub
struct A {
    pub foo
}
```

# Open function?

```
mod foo {
    open func a[T]();
}
```

```
// some other mod
// specify behavior of int
fact x func foo.a[int]() {
    print(1)
}
```

```
// specify behavior of T where T is Iter
fact y func foo.a[T: Iter]() {
    print(2)
}
```

```
use x & y // meaning use specificity rules
// use x ^ y // meaning error if ambiguous
let a = foo.a[int](); // prints 1
let b = foo.a[Array[]](); // prints 2
```

```
// traits are predicates on types
pred has_foo {
    // this means foo.a[Self]() should compile
    foo.a[Self]()
}
```

```
// select spec x
let a = use x in foo.a[int]()
```

问题：call site 和 def site 的同trait同class实现如果不同，那怎么办？
本质上这是相互矛盾的2个公理，一个说 `foo.a[int]` = `a` 一个说`foo.a[int]` = `b` 。没法解决这个问题。你不能说禁止使用def site 那里可见的公理，那样def自己根本啥也做不了。所以遇到冲突就无法解决。

唯一的办法是确保不会有冲突的公理。也就是说把实现限制在某个范围内。你检查好了没有冲突，然后就不允许外面再加实现了，这个trait或者class就封装死了。

结论：ORPHAN RULES 

# 比较有希望的方向：高阶类型，类型函数，类型模式

- Span type
- unwrap 整治
- function 多参数支持
