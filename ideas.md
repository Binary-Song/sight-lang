# 重载限制

原则上，不允许基于 具体性（Specificity）的重载。因为实现复杂且难以理解。只允许简单的重载。

# 同名符号规则

1. 每个模块只允许导出1个同名符号。

2. 从不同模块导入同名符号时，会发生报错。用户需要自定义另一个新的 函数来执行函数重载。

use mod1::*/add
use mod2::*/add

fn add[T](a: T, b: T)
{
    // 如果我们想要typecheck这个，那是不是要支持higher-order? 
    if T == Vector3 {
        mod1::add a b
    } else {
        mod2::add a b 
    }
}

# HM Type inference

1. 不支持Recursive Type Inference（但是支持Rec Type）如果涉及Recursive，必须明显标注类型。

# Pattern matching

1. 模式匹配的自定义本质是支持Destructor。也就是Constructor的相反，将一个value反向解析到一个容器里。

# Impl Transforming

Type可以像对象一样，经过一个“转换器”被转换成另一个type，在当前context替换原有的type，
转换器可以为struct添加impl实现。也可以覆盖已有的实现

# No Ad-hoc polymorphism

# Comp-Time Args

编译期参数。要求不能依赖任何运行时参数。

# Morphs as Comp-Time Args 

Morphs 是从类型指向 Trait 的箭头。


# Trait 是一种元编程

考虑给trait系统增加东西时，不要把它理解成类型系统的问题，而是理解成元编程的问题。

例如 blanket impl 实际上就是在生成代码，和 derive 并没有本质区别。

